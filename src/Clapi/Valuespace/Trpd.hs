{-# LANGUAGE
    DataKinds
  , GADTs
  , LambdaCase
#-}

module Clapi.Valuespace.Trpd where

import Control.Lens (_1, _2, _3, assign, modifying, use)
import Control.Monad (unless, void, when)
import Control.Monad.Except (throwError)
import Control.Monad.State (State, StateT, execState, get, put)
import Data.Bifunctor (bimap, first)
import Data.Either (isLeft)
import Data.Foldable (fold, toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Merge.Strict
  ( merge, dropMissing, mapMaybeMissing, zipWithMatched
  , mergeA, traverseMissing, zipWithAMatched
  )
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tagged (untag)
import Data.Text (Text)
import Text.Printf (printf)

import qualified Data.Map.Dependencies as Dependencies
import Data.Map.Mol (Mol)
import qualified Data.Map.Mol as Mol

import Clapi.Tree (RoseTree(..), RoseTreeNode(..), TimePoint, Attributed)
import qualified Clapi.Tree as Tree
import qualified Clapi.Types.AssocList as AL
import Clapi.Types.Base (Time, TpId, Attributee)
import Clapi.Types.Definitions
  ( Definition(..), MetaType(..), Editability, DefName, SomeDefinition(..))
import Clapi.Types.Digests
  ( DataErrorIndex(..), DefOp(..), isDef
  , TrDigest(..), FrDigest(..), TrpDigest, FrcUpdateDigest)
import qualified Clapi.Types.Dkmap as Dkmap
import Clapi.Types.Error (collect, errsStateT, eitherModifying)
import qualified Clapi.Types.Error as Error
import Clapi.Types.Path (Path, Name, pattern (:/))
import qualified Clapi.Types.Path as Path
import Clapi.Types.SequenceOps (SequenceOp, fullOrderOps)
import Clapi.Types.Wire (SomeWireValue)
import Clapi.Util (mapFoldMapMWithKey)
import Clapi.Validator (TypeAssertion(..))

import Clapi.Internal.Valuespace
  ( Valuespace, DefMap, TypeAssignmentMap
  , vsRootDefName, vsTree, vsTyDefs, vsPostDefs, vsTyAssns, vsTac)
import Clapi.Valuespace.Common (updatePathData, validateTupleValues)
import Clapi.Valuespace.Errors
  ( ErrText(..), AccessError(..), ErrorString(..), ProviderError(..)
  , StructuralError(..), ValidationError(..))
import Clapi.Valuespace.ErrWrap (Errs, Wraps(..), throw)
import Clapi.Valuespace.Prim
  ( VsM', lookupDef, pathDef, pathNode
  , pathError, pathErrors, tpErrors, castVsMError)
import qualified Clapi.Valuespace.Xrefs as Xrefs


-- FIXME: Perhaps this should return the Frped directly?
processTrpd
  :: Monad m
  => TrpDigest
  -> StateT Valuespace m (Either (Mol DataErrorIndex Text) FrcUpdateDigest)
processTrpd trpd = fmap (first $ fmap errText) $ do
  initState <- get
  e <- errsStateT $ processTrpd_ trpd
  when (isLeft e) $ put initState
  return e

processTrpd_ :: Monad m => TrpDigest -> VsM' ProviderError m FrcUpdateDigest
processTrpd_ trpd =
  let
    (pTyDefs, pTyUndefs) = partitionDefOps $ trpdPostDefs trpd
  in do
    defChanges <- classifyDefOps (trpdDefs trpd) <$> use vsTyDefs
    let (tyNewDefs, tyRedefs, tyUndefs) = partitionDefChanges defChanges
    oldTree <- use vsTree

    -- Now we know which types have been redefined vs newly defined, we can
    -- safely go ahead and update the `DefName -> Definition` mapping:
    modifying vsTyDefs
      $ Map.union tyNewDefs
      . Map.union (snd <$> tyRedefs)
      . flip Map.withoutKeys tyUndefs

    when (not $ null $ trpdDefs trpd) $
      use vsRootDefName >>= guardRecursiveStructs

    -- Let's update the rest of the primary state:
    modifying vsPostDefs $ Map.union pTyDefs . flip Map.withoutKeys pTyUndefs
    _ <- collect $ AL.fmapWithKey updatePathData $ trpdData trpd
    -- FIXME: The container updates might need to happen later, if changing the
    -- types has a material effect on what the types of the tree nodes are:
    _ <- collect $ Map.mapWithKey updateContainer $ trpdContOps trpd

    -- The tree should now be correct, so long as the provider has not made a
    -- mistake. So, next we need to check that is the case.

    -- Firstly we find all the type change implications that are a direct
    -- consequence of definition changes sent from the provider:
    roughImpls <- flip possibleImpls defChanges <$> use vsTyAssns

    -- Then we find the root changes for all the subtrees with changed
    -- types. For example, given the following tree where nodes with changed
    -- type are marked with ', we want to find only nodes /a/b and /a/c/f,
    -- marked with []:
    --
    --   a
    --   +-- [b']
    --   |   + -- d
    --   |   |     + -- g'
    --   |   |
    --   |   + -- e'
    --   |
    --   +-- c
    --       + -- [f']
    --
    let initialImpls = Path.prefixesMap roughImpls
    extendedImpls <- (initialImpls <>) <$> extendImpls oldTree initialImpls
    -- We add back any implications from the roughImpls that haven't been
    -- superceded by the recursive type implications:
    let allImpls = extendedImpls <> roughImpls
    _ <- collect $ uncurry handleImpl <$> Map.toDescList allImpls

    -- Finally, spit out the changes in a form that will be useful to the
    -- clients:
    return $ generateFrcud allImpls
  where
    generateFrcud allImpls = Frcud
      (trpdNs trpd)
      (trpdPostDefs trpd)
      (trpdDefs trpd)
      (Map.foldMapWithKey generateTyAsn allImpls)
      (trpdData trpd)
      (trpdContOps trpd)
      (trpdErrors trpd)

    generateTyAsn :: Path -> TypeImpl -> Map Path (DefName, Editability)
    generateTyAsn p = \case
      -- FIXME: Do we want to track editability changes through implications
      -- too?
      -- FIXME: We might be able to use the implication to minimise the
      -- definitions broadcast out to the clients too...
      TypeReassigned _ dn _ _ ed -> Map.singleton p (dn, ed)
      NewlyAssigned dn _ ed -> Map.singleton p (dn, ed)
      _ -> mempty


partitionDefOps :: Map k (DefOp a) -> (Map k a, Set k)
partitionDefOps = bimap (fmap odDef) Map.keysSet . Map.partition isDef


-- | Like DefOp, but takes into account previous state by separating new
--   definitions and redefinitions
data DefChange
  = NewDef SomeDefinition
  | Redef SomeDefinition SomeDefinition
  | Undef
  deriving Show

classifyDefOps
  :: Map DefName (DefOp SomeDefinition) -> Map DefName SomeDefinition
  -> Map DefName DefChange
classifyDefOps defOps oldTyDefs = merge
    dropMissing (mapMaybeMissing f) (zipWithMatched g) oldTyDefs defOps
  where
    f _dn = \case
      OpDefine newDef -> Just $ NewDef newDef
      OpUndefine -> Nothing
    g _dn oldDef = \case
      OpDefine newDef -> Redef oldDef newDef
      OpUndefine -> Undef


type DefDiff =
  ( DefMap SomeDefinition
  , Map DefName (SomeDefinition, SomeDefinition)
  , Set DefName)

partitionDefChanges :: Map DefName DefChange -> DefDiff
partitionDefChanges = flip execState mempty . sequence . Map.mapWithKey f
  where
    f :: DefName -> DefChange -> State DefDiff ()
    f dn = \case
      NewDef def -> modifying _1 $ Map.insert dn def
      Redef oldDef newDef -> modifying _2 $ Map.insert dn (oldDef, newDef)
      Undef -> modifying _3 $ Set.insert dn


-- | Protects against the case where a struct definition refers to itself
--   _without_ an intermediary array, which would result in an infinite tree.
guardRecursiveStructs
  :: (Errs '[AccessError, ProviderError] e, Monad m) => DefName -> VsM' e m ()
guardRecursiveStructs = go mempty []
  where
    go
      :: (Errs '[AccessError, ProviderError] e, Monad m)
      => Set DefName -> [DefName] -> DefName -> VsM' e m ()
    go processed structChainDns dn = unless (dn `Set.member` processed) $ do
      when (dn `elem` structChainDns) $ throwError $
        Mol.singleton GlobalError $ wrap $ CircularStructDefinitions $
        reverse $ dn : structChainDns
      SomeDefinition def <- castVsMError GlobalError $ lookupDef dn
      case def of
        TupleDef {} -> return ()
        StructDef { strDefChildTys = tyInfo } ->
          let dns = fst <$> toList tyInfo in
            mapM_ (go processed (dn : structChainDns)) dns
        -- We effectively "restart" our check if we hit an array:
        ArrayDef { arrDefChildTy = dn' } ->
          go (processed <> Set.fromList structChainDns) [] dn'

updateContainer
  :: (Errs '[AccessError, ErrorString, StructuralError] e, Monad m)
  => Path -> Map Name (Maybe Attributee, SequenceOp Name) -> VsM' e m ()
updateContainer p cOps = pathError p $ do
  SomeDefinition def <- pathDef p
  case def of
    ArrayDef {} -> eitherModifying vsTree
      $ first (wrap . ErrorString) . Tree.applyReorderingsAt p cOps
    -- FIXME: Potentially better error value here?
    _ -> throw SeqOpsOnNonArray


-- | Things that changes in the valuespace definitions can imply about a path
data TypeImpl
  = TypeRedefined DefName SomeDefinition SomeDefinition
  -- NB only new editability:
  -- FIXME: only need new def?
  | TypeReassigned DefName DefName SomeDefinition SomeDefinition Editability
  | NewlyAssigned DefName SomeDefinition Editability
  | ImplicitlyRemoved

instance Show TypeImpl where
  show = \case
    TypeRedefined dn _ _ -> printf "<TypeRedefined %s>" (show $ untag dn)
    TypeReassigned dn1 dn2 _ _ ed2 -> printf "<TypeReassigned %s %s %s>"
      (show $ untag dn1) (show $ untag dn2) (show ed2)
    NewlyAssigned dn _ ed -> printf "<NewlyAssigned %s %s>"
      (show $ untag dn) (show ed)
    ImplicitlyRemoved -> "<ImplicitlyRemoved>"

possibleImpls :: TypeAssignmentMap -> Map DefName DefChange -> Map Path TypeImpl
possibleImpls tyAssns = Map.foldMapWithKey f
  where
    f :: DefName -> DefChange -> Map Path TypeImpl
    f dn dc = case dc of
      NewDef _def -> mempty
      Redef oldDef newDef -> depPs dn $ TypeRedefined dn oldDef newDef
      Undef -> depPs dn ImplicitlyRemoved
    depPs dn x = Map.fromSet (const x) $ Dependencies.lookupRev dn tyAssns

extendImpls
  :: (Errs '[AccessError] e, Monad m)
  => RoseTree a -> Map Path TypeImpl -> VsM' e m (Map Path TypeImpl)
extendImpls oldTree initialImpls
  | Map.null initialImpls = return mempty
  | otherwise = do
      newImpls <- mapFoldMapMWithKey directChildImpls initialImpls
      recImpls <- extendImpls oldTree newImpls
      return $ newImpls <> recImpls
  where
    -- | Given something that happened to a path, what things happened directly
    --   below it?
    directChildImpls
      :: (Errs '[AccessError] e, Monad m)
      => Path -> TypeImpl -> VsM' e m (Map Path TypeImpl)
    directChildImpls p = \case
      TypeRedefined _ def1 def2 -> redefinitionDcImpls p def1 def2
      TypeReassigned _ _ def1 def2 _ -> redefinitionDcImpls p def1 def2
      NewlyAssigned _ def _ -> newDefDcImpls p def
      ImplicitlyRemoved -> return mempty

    redefinitionDcImpls
      :: (Errs '[AccessError] e, Monad m)
      => Path -> SomeDefinition -> SomeDefinition -> VsM' e m (Map Path TypeImpl)
    redefinitionDcImpls p (SomeDefinition oldDef) (SomeDefinition newDef) =
      let
        oldTyInfo = getChildTyInfo p oldDef oldTree
      in do
        newTyInfo <- getChildTyInfo p newDef <$> use vsTree
        tyInfoDiffDcImpls p oldTyInfo newTyInfo

    newDefDcImpls
      :: (Errs '[AccessError] e, Monad m)
      => Path -> SomeDefinition -> VsM' e m (Map Path TypeImpl)
    newDefDcImpls p (SomeDefinition def) = do
      newTyInfo <- getChildTyInfo p def <$> use vsTree
      tyInfoDiffDcImpls p mempty newTyInfo

    tyInfoDiffDcImpls
      :: (Errs '[AccessError] e, Monad m)
      => Path -> Map Name (DefName, Editability)
      -> Map Name (DefName, Editability) -> VsM' e m (Map Path TypeImpl)
    tyInfoDiffDcImpls p oldTyInfo newTyInfo = fold <$>
        mergeA (traverseMissing f) (traverseMissing g) (zipWithAMatched h)
        oldTyInfo newTyInfo
      where
        f n _ = return $ Map.singleton (p :/ n) ImplicitlyRemoved
        g n (dn, ed) = do
          def <- pathError p $ lookupDef dn
          return $ Map.singleton (p :/ n) $ NewlyAssigned dn def ed
        h n (dn1, ed1) (dn2, ed2)
          | dn1 == dn2 && ed1 == ed2 = return mempty
          | otherwise = do
              def1 <- pathError p $ lookupDef dn1
              def2 <- pathError p $ lookupDef dn2
              return $ Map.singleton (p :/ n) $
                TypeReassigned dn1 dn2 def1 def2 ed2

getChildTyInfo
  :: Path -> Definition mt -> RoseTree a -> Map Name (DefName, Editability)
getChildTyInfo p def = go . maybe RtnEmpty id . Tree.lookupNode p
  where
    go node = case def of
      TupleDef {} -> mempty
      StructDef { strDefChildTys = tyInfo } -> AL.toMap tyInfo
      ArrayDef { arrDefChildTy = dn, arrDefChildEd = ed } -> case node of
        RtnChildren al -> const (dn, ed) <$> AL.toMap al
        _ -> mempty

handleImpl
  :: ( Errs
       '[ AccessError, ErrorString, ProviderError, StructuralError
        , ValidationError] e
     , Monad m)
  => Path -> TypeImpl -> VsM' e m ()
handleImpl p = \case
    TypeRedefined _ _ (SomeDefinition def2) -> do
      inferContainer def2
      -- Revalidate the path according to def2
      -- FIXME: this is wasteful if the data has already been updated
      revalidatePath def2 p
    TypeReassigned _ dn2 _ (SomeDefinition def2) _ -> do
      -- Revalidate the path according to def2
      inferContainer def2
      modifying vsTyAssns $ Dependencies.insert p dn2
      -- FIXME: this is wasteful if the data has already been updated
      revalidatePath def2 p
      typeAssertions <- Xrefs.lookup (Xrefs.Referee p) <$> use vsTac
      revalidateXrefs dn2 typeAssertions
    NewlyAssigned dn (SomeDefinition def) _ -> do
      inferContainer def
      modifying vsTyAssns $ Dependencies.insert p dn
    ImplicitlyRemoved -> do
      modifying vsTree $ Tree.delete p
      modifying vsTyAssns $ Dependencies.delDependency p
      tac <- use vsTac
      let typeAssertions = Xrefs.lookup (Xrefs.Referee p) tac
      if null typeAssertions
        then assign vsTac $
          Xrefs.removeConst (Xrefs.Referer p) tac
        else
          pathError p $ throw $ RemovedWhileReferencedBy $
            Xrefs.referers (Xrefs.Referee p) tac
  where
    inferContainer
      :: (Errs '[AccessError, ErrorString, ProviderError] e, Monad m)
      => Definition mt -> VsM' e m ()
    inferContainer = \case
      TupleDef {} -> do
        node <- pathError p $ pathNode p
        case node of
          RtnEmpty -> pathError p $ throw MissingNodeData
          _ -> return ()
      -- The tree at this path should be a container, so we can infer that if
      -- the provider has not set it explicitly:
      StructDef { strDefChildTys = tyInfo } -> do
        modifying vsTree $ Tree.initContainerAt p
        pathError p $ eitherModifying vsTree $ fmap (first wrap) .
          Tree.applyReorderingsAt @(Either ErrorString) p $
          (Nothing,) <$> fullOrderOps (AL.keys_ tyInfo)
      ArrayDef {} -> modifying vsTree $ Tree.initContainerAt p


revalidatePath
  :: ( Errs '[AccessError, ErrorString, StructuralError, ValidationError] e
     , Monad m)
  => Definition mt -> Path -> VsM' e m ()
revalidatePath def path = case def of
  TupleDef {} -> revalidatePathData def path
  -- Container keys are guaranteed to be correct by the type inference:
  _ -> return ()

revalidatePathData
  :: ( Errs '[AccessError, ErrorString, StructuralError, ValidationError] e
     , Monad m)
  => Definition 'Tuple -> Path -> VsM' e m ()
revalidatePathData def@(TupleDef {tupDefILimit = ilimit}) p = case ilimit of
    Nothing -> goRevalidate $ revalidateConstData def p
    _ -> goRevalidate $ revalidateTsData def p
  where
    -- FIXME: This is currently modifying the tree because my previous stab had
    -- the valuespace holding a RoseTree containing TreeValues:
    -- FIXME: It would be nice not to lose container attribution just because we
    -- want to recheck some of the data!
    goRevalidate f = Error.modifying vsTree $ Tree.alterF Nothing (atNode f) p
    atNode f = \case
      Just rtn -> Just <$> f rtn
      Nothing -> pathError p $ throw NodeNotFound

revalidateConstData
  :: ( Errs '[AccessError, ErrorString, StructuralError, ValidationError] e
     , Monad m)
  => Definition 'Tuple -> Path -> RoseTree [SomeWireValue]
  -> VsM' e m (RoseTree [SomeWireValue])
revalidateConstData tdef p = \case
  rt@(RtConstData _att wvs) -> do
    tyAsserts <- pathErrors p (validateTupleValues tdef wvs)
    modifying vsTac $ Xrefs.updateConst (Xrefs.Referer p) tyAsserts
    return rt
  _ -> pathError p $ throw UnexpectedNodeType

revalidateTsData
  :: ( Errs '[AccessError, ErrorString, StructuralError, ValidationError] e
     , Monad m)
  => Definition 'Tuple -> Path -> RoseTree [SomeWireValue]
  -> VsM' e m (RoseTree [SomeWireValue])
revalidateTsData tdef p = \case
    rt@(RtDataSeries ts) -> collect (Dkmap.mapWithKeys atTp ts) >> return rt
    _ -> pathError p $ throw UnexpectedNodeType
  where
    atTp
        :: (Errs '[AccessError, ErrorString, ValidationError] e, Monad m)
        => TpId -> Time -> Attributed (TimePoint [SomeWireValue])
        -> VsM' e m ()
    atTp tpid _ (_, (_, wvs)) = do
      tyAsserts <- tpErrors p tpid $ validateTupleValues tdef wvs
      modifying vsTac $ Xrefs.updateTp (Xrefs.Referer p) tpid tyAsserts


-- FIXME: these are named incorrectly and should be something to do with
-- checking type assertions without clashing with anything checkTypeAssertiony
-- above!
revalidateXrefs
  :: (Errs '[ValidationError] e, Monad m)
  => DefName -> Map Xrefs.Referer TypeAssertion -> VsM' e m ()
revalidateXrefs actDn = void . collect . Map.mapWithKey (revalidateXref actDn)

revalidateXref
  :: (Errs '[ValidationError] e, Monad m)
  => DefName -> Xrefs.Referer -> TypeAssertion -> VsM' e m ()
revalidateXref actDn referer (TypeAssertion _ expDn) =
  pathError (Xrefs.unReferer referer) $
    unless (actDn == expDn) $ throw $ XRefError expDn actDn

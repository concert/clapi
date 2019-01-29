{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , LambdaCase
  , OverloadedStrings
#-}

module Clapi.Valuespace2
  ( Valuespace, baseValuespace
  , processTrpd
  ) where

import Control.Lens (_1, _2, _3, assign, modifying, use)
import Control.Monad (unless, void, when)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.Except (MonadError, ExceptT, runExceptT, throwError)
import Control.Monad.State (State, execState, StateT, get, put)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (tell)
import Data.Bifunctor (first, bimap)
import Data.Either (isLeft)
import Data.Foldable (fold, toList)
import Data.List (intercalate)
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
import qualified Data.Text as Text
import Text.Printf (printf)

import qualified Data.Map.Dependencies as Dependencies
import Data.Map.Mol (Mol)
import qualified Data.Map.Mol as Mol
import Data.Map.Mos (Mos)
import qualified Data.Map.Mos as Mos

import Clapi.Internal.Valuespace hiding (Referer, Referee)
import Clapi.Tree (RoseTree(..), RoseTreeNode(..), TimePoint, Attributed)
import qualified Clapi.Tree as Tree
import qualified Clapi.Types.AssocList as AL
import Clapi.Types.Base (Time, InterpolationType(..), Attributee, TypeEnumOf(..))
import Clapi.Types.Definitions
  ( MetaType(..), SomeDefinition(..), Definition(..), PostDefinition(..)
  , Editability
  , withDefinition
  , structDef
  , DefName, PostDefName
  , getTyInfoForSeg
  )
import Clapi.Types.Digests
  ( DataErrorIndex(..), DefOp(..), isDef, DataChange(..), isRemove
  , TimeSeriesDataOp(..), TpId
  , TrDigest(..), FrDigest(..), TrpDigest, FrcUpdateDigest)
import qualified Clapi.Types.Dkmap as Dkmap
import Clapi.Types.Error
  (ErrsT, errsStateT, castErrs, collect, eitherThrow, eitherModifying)
import qualified Clapi.Types.Error as Error
import Clapi.Types.Path
  (Path, Seg, pattern Root, pattern (:/), pattern (:</))
import qualified Clapi.Types.Path as Path
import Clapi.Types.SequenceOps (SequenceOp(..), fullOrderOps)
import Clapi.Types.UniqList (unUniqList)
import Clapi.Types.Wire (SomeWireValue)
import Clapi.Util (mapFoldMapMWithKey, foldMapM)
import Clapi.Validator (TypeAssertion(..), validateValues)
import qualified Clapi.Valuespace2.Xrefs as Vs2Xrefs


-- FIXME: I think there will end up being ProviderErrors that we make when we
-- try to apply an Frpd and then a ClientErrors that we make when we try to
-- apply a client update digest.
data ProviderError
  = NodeNotFound
  | DefNotFound DefName
  | PostDefNotFound PostDefName
  | CircularStructDefinitions [DefName]
  | ExpectedArrayDefinition
  | ArrayDoesNotSupportCreates
  | MissingNodeData
  | UnexpectedNodeType
  | BadChildKeys [Seg] [Seg]
  | TsChangeOnConst | ConstChangeOnTs
  -- FIXME: this really should contain two InterpolationType values, but because
  -- we currently don't distinguish between TS tuples and const tuples at the
  -- type level, the interpolation type we were expecting is technically
  -- optional:
  | BadInterpolationType InterpolationType (Maybe InterpolationType)
  | XRefError DefName DefName
  | XRefError2 DefName DefName
  | RemovedWhileReferencedBy (Set (Vs2Xrefs.Referer, Maybe TpId))
  | ValidationError String
  | ErrorString String

errText :: ProviderError -> Text
errText = Text.pack . \case
  NodeNotFound -> "Node not found"
  DefNotFound dn -> printf "Definition %s not found" $ show $ untag dn
  PostDefNotFound dn -> printf "Post definition %s not found" $ show $ untag dn
  CircularStructDefinitions dns ->
    "Circular struct definitions: " ++ intercalate " -> " (show . untag <$> dns)
  ExpectedArrayDefinition -> "Expected array definition"
  ArrayDoesNotSupportCreates -> "Array does not support creates"
  MissingNodeData -> "Missing node data"
  UnexpectedNodeType -> "Unexpected node type"
  BadChildKeys ac ex -> printf
    "Bad child keys: %s (expected %s)" (show ac) (show ex)
  TsChangeOnConst -> "Time series change on constant data node"
  ConstChangeOnTs -> "Constant data change on time series data node"
  BadInterpolationType actual expected -> printf
    "Bad interpolation type %s. Expected <= %s" (show actual) (show expected)
  XRefError expDn actDn -> printf
    "Bad xref target type. Expected %s, got %s" (show expDn) (show actDn)
  XRefError2 _ _ -> "debug"
  RemovedWhileReferencedBy referers -> printf "Removed path referenced by %s"
    (show referers)
  ValidationError s -> "ValidationError: " ++ s
  ErrorString s -> s

instance MonadFail (Either ProviderError) where
  fail = Left . ErrorString

type VsM m = ExceptT ProviderError (StateT Valuespace m)
type VsM' m = ErrsT Valuespace (Mol DataErrorIndex ProviderError) m

castVsMError :: Monad m => DataErrorIndex -> VsM m a -> VsM' m a
castVsMError idx m =
  (lift $ lift $ runExceptT m) >>=
  either (throwError . Mol.singleton idx) return

pathError :: Monad m => Path -> VsM m a -> VsM' m a
pathError p = castVsMError $ PathError p

tpError :: Monad m => Path -> TpId -> VsM m a -> VsM' m a
tpError p tpid = castVsMError $ TimePointError p tpid

pathErrors
  :: Monad m
  => Path -> ErrsT Valuespace [e] m a
  -> ErrsT Valuespace (Mol DataErrorIndex e) m a
pathErrors p = castErrs $ Mol.singletonList $ PathError p

tpErrors
  :: Monad m
  => Path -> TpId -> ErrsT Valuespace [e] m a
  -> ErrsT Valuespace (Mol DataErrorIndex e) m a
tpErrors p tpid = castErrs $ Mol.singletonList $ TimePointError p tpid

note :: MonadError e m => e -> Maybe a -> m a
note err = maybe (throwError err) return

lookupDef :: Monad m => DefName -> VsM m SomeDefinition
lookupDef dn = use vsTyDefs >>= note (DefNotFound dn) . Map.lookup dn

lookupPostDef :: Monad m => PostDefName -> VsM m PostDefinition
lookupPostDef dn = use vsPostDefs >>= note (PostDefNotFound dn) . Map.lookup dn

pathTyInfo :: Monad m => Path -> VsM m (DefName, Editability)
pathTyInfo path = do
    dn <- use vsRootDefName
    ed <- use vsRootEditability
    go path (dn, ed)
  where
    go
      :: Monad m
      => Path -> (DefName, Editability) -> VsM m (DefName, Editability)
    go (s :</ p) (dn, _) = do
      SomeDefinition def <- lookupDef dn
      r <- eitherThrow $ getTyInfoForSeg s def
      go p r
    go _ r = return r

pathDef :: Monad m => Path -> VsM m SomeDefinition
pathDef path = pathTyInfo path >>= lookupDef . fst

pathPostDef :: Monad m => Path -> VsM m PostDefinition
pathPostDef path = do
  SomeDefinition def <- pathDef path
  case def of
    ArrayDef { arrDefPostTy = mpd } -> maybe
      (throwError ArrayDoesNotSupportCreates) lookupPostDef mpd
    _ -> throwError ExpectedArrayDefinition

pathDefName :: Monad m => Path -> VsM m DefName
pathDefName path = fst <$> pathTyInfo path

pathEditability :: Monad m => Path -> VsM m Editability
pathEditability path = snd <$> pathTyInfo path

pathNode :: Monad m => Path -> VsM m (RoseTreeNode [SomeWireValue])
pathNode path = use vsTree >>= guard . Tree.lookupNode path
  where
    guard Nothing = throwError NodeNotFound
    guard (Just n) = case n of
      -- FIXME: More evidence for getting rid of RtnEmpty?
      RtnEmpty -> throwError NodeNotFound
      _ -> return n

pathChildren :: Monad m => Path -> VsM m [Seg]
pathChildren path = pathNode path >>= return . \case
  RtnChildren al -> unUniqList $ AL.alKeys al
  _ -> mempty


baseValuespace :: DefName -> Editability -> Valuespace
baseValuespace rootDn rootEd = Valuespace
    (Tree.RtContainer AL.alEmpty)
    mempty
    (Map.singleton rootDn emptyStructDef)
    rootDn
    rootEd
    (Dependencies.singleton Root rootDn)
    mempty
    Vs2Xrefs.emptyTac
  where
    emptyStructDef = structDef "Empty namespace" AL.alEmpty


processTrpd
  :: Monad m
  => TrpDigest
  -> StateT Valuespace m (Either (Mol DataErrorIndex Text) FrcUpdateDigest)
processTrpd trpd = fmap (first $ fmap errText) $ do
  initState <- get
  e <- errsStateT $ processTrpd_ trpd
  when (isLeft e) $ put initState
  return e

processTrpd_ :: Monad m => TrpDigest -> VsM' m FrcUpdateDigest
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
      use vsRootDefName >>= guardRecusiveStructs

    -- Let's update the rest of the primary state:
    modifying vsPostDefs $ Map.union pTyDefs . flip Map.withoutKeys pTyUndefs
    collect $ AL.alFmapWithKey updatePathData $ trpdData trpd
    -- FIXME: The container updates might need to happen later, if changing the
    -- types has a material effect on what the types of the tree nodes are:
    collect $ Map.mapWithKey updateContainer $ trpdContOps trpd

    -- tac <- use vsTac
    -- traceShow tac $ return ()

    -- The tree should now be correct, so long as the provider has not made a
    -- mistake. So, next we need to check that is the case.
    roughImpls <- flip possibleImpls defChanges <$> use vsTyAssns
    -- These are the top-level type change implications that we _know_ will need
    -- to be checked:
    let initialImpls = Path.prefixesMap roughImpls
    extendedImpls <- (initialImpls <>) <$> extendImpls oldTree initialImpls
    -- We add back any implications from the roughImpls that haven't been
    -- superceded by the recursive type implications:
    let allImpls = extendedImpls <> roughImpls
    collect $ uncurry handleImpl <$> Map.toDescList allImpls

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


-- | Like DefOp, but takes into account previous state by separating new
--   definitions and redefinitions
data DefChange
  = NewDef SomeDefinition
  | Redef SomeDefinition SomeDefinition
  | Undef
  deriving Show

type DefDiff =
  ( DefMap SomeDefinition
  , Map DefName (SomeDefinition, SomeDefinition)
  , Set DefName)

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


partitionDefOps :: Map k (DefOp a) -> (Map k a, Set k)
partitionDefOps = bimap (fmap odDef) Map.keysSet . Map.partition isDef

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

partitionDefChanges :: Map DefName DefChange -> DefDiff
partitionDefChanges = flip execState mempty . sequence . Map.mapWithKey f
  where
    f :: DefName -> DefChange -> State DefDiff ()
    f dn = \case
      NewDef def -> modifying _1 $ Map.insert dn def
      Redef oldDef newDef -> modifying _2 $ Map.insert dn (oldDef, newDef)
      Undef -> modifying _3 $ Set.insert dn


updatePathData :: Monad m => Path -> DataChange -> VsM' m ()
updatePathData p dc = do
    SomeDefinition def <- pathError p $ pathDef p
    case def of
      TupleDef {tupDefILimit = ilimit } -> case ilimit of
        Nothing -> applyConstChange def dc
        _ -> applyTsChanges def dc
      _ -> pathError p $ throwError UnexpectedNodeType
  where
    applyConstChange :: Monad m => Definition 'Tuple -> DataChange -> VsM' m ()
    applyConstChange tdef = \case
      TimeChange {} -> pathError p $ throwError TsChangeOnConst
      ConstChange att wvs -> pathErrors p $ do
        tyAsserts <- validateTupleValues tdef wvs
        modifying vsTree $ Tree.constSetAt att p wvs
        modifying vsTac $ Vs2Xrefs.updateConstTas (Vs2Xrefs.Referer p) tyAsserts

    applyTsChanges :: Monad m => Definition 'Tuple -> DataChange -> VsM' m ()
    applyTsChanges tdef = \case
      TimeChange m -> void $ collect $ Map.mapWithKey (applyTpChange tdef) m
      ConstChange {} -> pathError p $ throwError ConstChangeOnTs

    applyTpChange
      :: Monad m
      => Definition 'Tuple -> TpId -> (Maybe Attributee, TimeSeriesDataOp)
      -> VsM' m ()
    applyTpChange tdef tpid (att, tdOp) = tpErrors p tpid $ case tdOp of
      OpSet t wvs i -> do
        unless (Just (typeEnumOf i) <= tupDefILimit tdef) $
          throwError [BadInterpolationType (typeEnumOf i) (tupDefILimit tdef)]
        tyAsserts <- validateTupleValues tdef wvs
        eitherModifying vsTree $
          first (\s -> [ErrorString s]) . Tree.setTpAt att p tpid t wvs i
        modifying vsTac $
          Vs2Xrefs.updateTpTas (Vs2Xrefs.Referer p) tpid tyAsserts
      OpRemove -> do
        eitherModifying vsTree $
          first (\s -> [ErrorString s]) . Tree.removeTpAt att p tpid
        modifying vsTac $ Vs2Xrefs.removeTpTas (Vs2Xrefs.Referer p) tpid

updateContainer
  :: Monad m => Path -> Map Seg (Maybe Attributee, SequenceOp Seg) -> VsM' m ()
updateContainer p cOps = pathError p $
  eitherModifying vsTree $ first ErrorString . Tree.applyReorderingsAt p cOps

-- | Protects against the case where a struct definition refers to itself
--   _without_ an intermediary array, which would result in an infinite tree.
guardRecusiveStructs :: Monad m => DefName -> VsM' m ()
guardRecusiveStructs = go mempty []
  where
    go :: Monad m => Set DefName -> [DefName] -> DefName -> VsM' m ()
    go processed structChainDns dn = unless (dn `Set.member` processed) $ do
      when (dn `elem` structChainDns) $ throwError $
        Mol.singleton GlobalError $ CircularStructDefinitions $
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
  :: Monad m => RoseTree a -> Map Path TypeImpl -> VsM' m (Map Path TypeImpl)
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
      :: Monad m => Path -> TypeImpl -> VsM' m (Map Path TypeImpl)
    directChildImpls p = \case
      TypeRedefined _ def1 def2 -> redefinitionDcImpls p def1 def2
      TypeReassigned _ _ def1 def2 _ -> redefinitionDcImpls p def1 def2
      NewlyAssigned _ def _ -> newDefDcImpls p def
      ImplicitlyRemoved -> return mempty

    redefinitionDcImpls
      :: Monad m
      => Path -> SomeDefinition -> SomeDefinition -> VsM' m (Map Path TypeImpl)
    redefinitionDcImpls p (SomeDefinition oldDef) (SomeDefinition newDef) =
      let
        oldTyInfo = getChildTyInfo p oldDef oldTree
      in do
        newTyInfo <- getChildTyInfo p newDef <$> use vsTree
        tyInfoDiffDcImpls p oldTyInfo newTyInfo

    newDefDcImpls
      :: Monad m => Path -> SomeDefinition -> VsM' m (Map Path TypeImpl)
    newDefDcImpls p (SomeDefinition def) = do
      newTyInfo <- getChildTyInfo p def <$> use vsTree
      tyInfoDiffDcImpls p mempty newTyInfo

    tyInfoDiffDcImpls
      :: Monad m
      => Path -> Map Seg (DefName, Editability)
      -> Map Seg (DefName, Editability) -> VsM' m (Map Path TypeImpl)
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
  :: Path -> Definition mt -> RoseTree a -> Map Seg (DefName, Editability)
getChildTyInfo p def = go . maybe RtnEmpty id . Tree.lookupNode p
  where
    go node = case def of
      TupleDef {} -> mempty
      StructDef { strDefChildTys = tyInfo } -> AL.alToMap tyInfo
      ArrayDef { arrDefChildTy = dn, arrDefChildEd = ed } -> case node of
        RtnChildren al -> const (dn, ed) <$> AL.alToMap al
        _ -> mempty


handleImpl :: Monad m => Path -> TypeImpl -> VsM' m ()
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
      typeAssertions <- Vs2Xrefs.lookup (Vs2Xrefs.Referee p) <$> use vsTac
      revalidateXrefs dn2 typeAssertions
    NewlyAssigned dn (SomeDefinition def) _ -> do
      inferContainer def
      modifying vsTyAssns $ Dependencies.insert p dn
    ImplicitlyRemoved -> do
      modifying vsTree $ Tree.delete p
      modifying vsTyAssns $ Dependencies.delDependency p
      tac <- use vsTac
      let typeAssertions = Vs2Xrefs.lookup (Vs2Xrefs.Referee p) tac
      if null typeAssertions
        then assign vsTac $
          Vs2Xrefs.removeTas (Vs2Xrefs.Referer p) tac
        else
          pathError p $ throwError $ RemovedWhileReferencedBy $
            Vs2Xrefs.referers (Vs2Xrefs.Referee p) tac
  where
    inferContainer :: Monad m => Definition mt -> VsM' m ()
    inferContainer = \case
      TupleDef {} -> do
        node <- pathError p $ pathNode p
        case node of
          RtnEmpty -> pathError p $ throwError MissingNodeData
          _ -> return ()
      -- The tree at this path should be a container, so we can infer that if
      -- the provider has not set it explicitly:
      StructDef { strDefChildTys = tyInfo } -> do
        modifying vsTree $ Tree.initContainerAt p
        pathError p $ eitherModifying vsTree $ Tree.applyReorderingsAt p $
          (Nothing,) <$> fullOrderOps (AL.alKeys_ tyInfo)
      ArrayDef {} -> modifying vsTree $ Tree.initContainerAt p


validateTupleValues
  :: Monad m
  => Definition 'Tuple -> [SomeWireValue]
  -> ErrsT Valuespace [ProviderError] m Vs2Xrefs.TypeAssertions
validateTupleValues tdef wvs = do
  tas <- eitherThrow
    $ first (fmap ValidationError)
    $ validateValues (toList $ tupDefTys tdef) wvs
  checkTypeAssertions tas

checkTypeAssertions
  :: Monad m
  => Set TypeAssertion
  -> ErrsT Valuespace [ProviderError] m Vs2Xrefs.TypeAssertions
checkTypeAssertions tas = mapM_ checkTypeAssertion tas
  >> return (Vs2Xrefs.toTypeAssertions tas)

-- FIXME: naming is confusing between this one that grabs the actDn and the
-- actual check which takes actDn:
checkTypeAssertion
  :: Monad m => TypeAssertion -> ErrsT Valuespace [ProviderError] m ()
checkTypeAssertion (TypeAssertion path expDn) = casty $ do
    actDn <- pathDefName path
    unless (actDn == expDn) $ throwError $ XRefError expDn actDn
  where
    casty
      :: Monad m
      => ExceptT ProviderError (StateT Valuespace m) ()
      -> ErrsT Valuespace [ProviderError] m ()
    casty m = (lift $ lift $ runExceptT m) >>= either (throwError . pure) return



revalidatePath :: Monad m => Definition mt -> Path -> VsM' m ()
revalidatePath def path = case def of
  TupleDef {} -> revalidatePathData def path
  -- Container keys are guaranteed to be correct by the type inference:
  _ -> return ()
  -- StructDef {} -> revalidateContainerKeys def
  -- ArrayDef {} -> revalidateContainerKeys def

-- revalidateContainerKeys :: Monad m => Definition mt -> Path -> VsM' m ()
-- revalidateContainerKeys def path = pathError path $ do
--   node <- pathNode path
--   case node of
--     RtnChildren al -> case def of
--       StructDef { strDefChildTys = tyInfo } ->
--         let expected = AL.alKeys tyInfo; actual = AL.alKeys al in
--         unless (actual == expected) $
--           throwError $ BadChildKeys (unUniqList actual) (unUniqList expected)
--       ArrayDef {} -> return ()
--       TupleDef {} -> error "Should not call with TupleDef"
--     _ -> throwError UnexpectedNodeType

revalidatePathData :: Monad m => Definition 'Tuple -> Path -> VsM' m ()
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
      Nothing -> pathError p $ throwError NodeNotFound

revalidateConstData
  :: Monad m
  => Definition 'Tuple -> Path -> RoseTree [SomeWireValue]
  -> VsM' m (RoseTree [SomeWireValue])
revalidateConstData tdef p = \case
  rt@(RtConstData _att wvs) -> do
    tyAsserts <- pathErrors p (validateTupleValues tdef wvs)
    modifying vsTac $ Vs2Xrefs.updateConstTas (Vs2Xrefs.Referer p) tyAsserts
    return rt
  _ -> pathError p $ throwError UnexpectedNodeType

revalidateTsData
  :: Monad m
  => Definition 'Tuple -> Path -> RoseTree [SomeWireValue]
  -> VsM' m (RoseTree [SomeWireValue])
revalidateTsData tdef p = \case
    rt@(RtDataSeries ts) -> collect (Dkmap.mapWithKeys atTp ts) >> return rt
    _ -> pathError p $ throwError UnexpectedNodeType
  where
    atTp
        :: Monad m
        => TpId -> Time -> Attributed (TimePoint [SomeWireValue])
        -> VsM' m ()
    atTp tpid _ (_, (_, wvs)) = do
      tyAsserts <- tpErrors p tpid $ validateTupleValues tdef wvs
      modifying vsTac $ Vs2Xrefs.updateTpTas (Vs2Xrefs.Referer p) tpid tyAsserts


-- FIXME: these are names incorrectly and should be something to do with
-- checking type assertions without clashing with anything checkTypeAssertiony
-- above!
revalidateXrefs
  :: Monad m => DefName -> Map Vs2Xrefs.Referer TypeAssertion -> VsM' m ()
revalidateXrefs actDn = void . collect . Map.mapWithKey (revalidateXref actDn)

revalidateXref
  :: Monad m => DefName -> Vs2Xrefs.Referer -> TypeAssertion -> VsM' m ()
revalidateXref actDn referer (TypeAssertion _ expDn) =
  pathError (Vs2Xrefs.unReferer referer) $
    unless (actDn == expDn) $ throwError $ XRefError2 expDn actDn

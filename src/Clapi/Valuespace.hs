{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  , PartialTypeSignatures
#-}

module Clapi.Valuespace
  ( Valuespace, baseValuespace
  , processTrpd, processTrcud

  , ProviderError
  , lookupPostDef, lookupDef, pathNode, pathTyInfo
  ) where

import Control.Lens (_1, _2, _3, assign, modifying, use)
import Control.Monad (join, unless, void, when, (>=>))
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import Control.Monad.State (MonadState(..), State, execState, StateT, get, put)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (MonadWriter(..))
import Data.Bifunctor (first, bimap)
import Data.Either (isLeft)
import Data.Foldable (fold, foldl', toList)
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
import Data.Map.Mos (Mos, unMos)
import qualified Data.Map.Mos as Mos

import Clapi.Internal.Valuespace hiding (Referer, Referee)
import Clapi.Tree (RoseTree(..), RoseTreeNode(..), TimePoint, Attributed)
import qualified Clapi.Tree as Tree
import qualified Clapi.Types.AssocList as AL
import Clapi.Types.Base
  (TpId, Time, InterpolationType(..), Attributee, TypeEnumOf(..))
import Clapi.Types.Definitions
  ( MetaType(..), SomeDefinition(..), Definition(..), PostDefinition(..)
  , Editability(..)
  , structDef
  , DefName, PostDefName
  , getTyInfoForSeg
  )
import Clapi.Types.Digests
  ( DataErrorIndex(..), DefOp(..), isDef, DataChange(..)
  , TimeSeriesDataOp(..), Creates, CreateOp(..), ContOps, DataDigest
  , TrDigest(..), FrDigest(..)
  , TrpDigest, FrcUpdateDigest
  , TrcUpdateDigest, FrpDigest, frpdEmpty)
import qualified Clapi.Types.Dkmap as Dkmap
import Clapi.Types.Error
  (ErrsT, errsStateT, castErrs, collect, eitherThrow, eitherModifying)
import qualified Clapi.Types.Error as Error
import Clapi.Types.Path
  (Path, Seg, Placeholder,  pattern Root, pattern (:/), pattern (:</))
import qualified Clapi.Types.Path as Path
import Clapi.Types.SequenceOps (SequenceOp(..), fullOrderOps)
import Clapi.Types.UniqList (unUniqList)
import Clapi.Types.Wire (SomeWireValue)
import Clapi.Util
  ( mapFoldMapMWithKey, justs, lefts, strictZipWith, fmtStrictZipError)
import Clapi.Validator (TypeAssertion(..), validateValues)
import qualified Clapi.Valuespace.Xrefs as Vs2Xrefs
import Clapi.Valuespace.ErrWrap
  (Wraps(..), Errs, MonadErrors, throw, liftEither)


class ErrText e where
  errText :: e -> Text

newtype ErrorString = ErrorString { unErrorString :: String }

data AccessError
  = NodeNotFound
  | DefNotFound DefName
  | PostDefNotFound PostDefName

instance ErrText AccessError where
  errText = Text.pack . \case
    NodeNotFound -> "Node not found"
    DefNotFound dn -> printf "Definition %s not found" $ show $ untag dn
    PostDefNotFound dn ->
      printf "Post definition %s not found" $ show $ untag dn

data ValidationError
  -- FIXME: DataValidationError is a wrapper for MonadFail stuff that comes out
  -- of validation. It would be better to have a whole type for validation
  -- errors that can come from the Validation module:
  = DataValidationError String
  | XRefError DefName DefName
  -- FIXME: this really should contain two InterpolationType values, but because
  -- we currently don't distinguish between TS tuples and const tuples at the
  -- type level, the interpolation type we were expecting is technically
  -- optional:
  | BadInterpolationType InterpolationType (Maybe InterpolationType)

instance ErrText ValidationError where
  errText = Text.pack . \case
    DataValidationError s -> "ValidationError: " ++ s
    XRefError expDn actDn -> printf
      "Bad xref target type. Expected %s, got %s" (show expDn) (show actDn)
    BadInterpolationType actual expected -> printf
      "Bad interpolation type %s. Expected <= %s" (show actual) (show expected)


-- | Raised when client's attempts to mutate the data in the tree is
--   inconsistent with the structure of the tree.
data StructuralError
  = TsChangeOnConst | ConstChangeOnTs
  | UnexpectedNodeType
  -- FIXME: Check that this can be raised by both Consumers and Providers:
  | SeqOpsOnNonArray

instance ErrText StructuralError where
  errText = Text.pack . \case
    TsChangeOnConst -> "Time series change on constant data node"
    ConstChangeOnTs -> "Constant data change on time series data node"
    UnexpectedNodeType -> "Unexpected node type"
    SeqOpsOnNonArray -> "Array rearrangement operation on non-array"


data SeqOpError soTarget
  -- FIXME: Check both Consumers and Providers can raise these
  = SeqOpMovedMissingChild Seg
  | SeqOpTargetMissing Seg soTarget

instance Show soTarget => ErrText (SeqOpError soTarget) where
  errText = Text.pack . \case
    SeqOpMovedMissingChild kid -> printf
      "Array rearrangment attempted to move missing member %s" (show kid)
    SeqOpTargetMissing seg eps -> printf
      "Array rearrangment attempt to move member %s after non-existent target %s"
      (show seg) (show eps)



-- FIXME: I think there will end up being ProviderErrors that we make when we
-- try to apply an Frpd and then a ConsumerErrors that we make when we try to
-- apply a client update digest.
-- FIXME: There may even be a common subset for things that both the API
-- provider and the consumer can get wrong. For example, referencing missing
-- nodes.
data ProviderError
  = PEAccessError AccessError
  | CircularStructDefinitions [DefName]
  | MissingNodeData
  | PEValidationError ValidationError
  | PEStructuralError StructuralError
  | PESeqOpError (SeqOpError Seg)
  -- FIXME: This is currently only exposed when handling implicit removes from
  -- Provider definition updates, but we could potentially be invalid if a
  -- Consumer drops a member of an array!
  | RemovedWhileReferencedBy (Set (Vs2Xrefs.Referer, Maybe TpId))
  -- FIXME: ErrorString is too generic, as it loses where the error came
  -- from. It is _not_ a wrapper for errors that were generated by providers, so
  -- we should be able to be much more specific!
  | PEErrorString String

instance ErrText ProviderError where
  errText = Text.pack . \case
    PEAccessError ae -> Text.unpack $ errText ae
    PEStructuralError se -> Text.unpack $ errText se
    PESeqOpError soe -> Text.unpack $ errText soe
    CircularStructDefinitions dns ->
      "Circular struct definitions: "
      ++ intercalate " -> " (show . untag <$> dns)
    MissingNodeData -> "Missing node data"
    RemovedWhileReferencedBy referers -> printf "Removed path referenced by %s"
      (show referers)
    PEValidationError ve -> Text.unpack $ errText ve
    PEErrorString s -> s

instance Wraps AccessError ProviderError where
  wrap = PEAccessError

instance Wraps ValidationError ProviderError where
  wrap = PEValidationError

instance Wraps StructuralError ProviderError where
  wrap = PEStructuralError

instance Wraps (SeqOpError Seg) ProviderError where
  wrap = PESeqOpError

instance Wraps ErrorString ProviderError where
  wrap = PEErrorString . unErrorString


data ConsumerError
  = CEAccessError AccessError
  | CEStructuralError StructuralError
  | CESeqOpError (SeqOpError EPS)
  | CEValidationError ValidationError
  | ReadOnlyEdit
  | ReadOnlySeqOps  -- FIXME: potentially combine with ReadOnlyEdit
  | CreatesNotSupported
  | MultipleCreatesReferencedTarget (Set Placeholder) (Maybe EPS)
  | CyclicReferencesInCreates [Placeholder]
  | MissingCreatePositionTarget Placeholder EPS
  | CEErrorString String

instance ErrText ConsumerError where
  errText = Text.pack . \case
    CEAccessError ae -> Text.unpack $ errText ae
    CEValidationError ve -> Text.unpack $ errText ve
    CEStructuralError se -> Text.unpack $ errText se
    CESeqOpError soe -> Text.unpack $ errText soe
    ReadOnlyEdit -> "Data change at read-only path"
    ReadOnlySeqOps -> "Tried to alter the children of a read-only path"
    CreatesNotSupported -> "Creates not supported"
    MultipleCreatesReferencedTarget phs targ -> printf
      "Multiple create operations (%s) referernced the same position target (%s)"
      (show phs) (show targ)
    CyclicReferencesInCreates targs ->
      "Several create operations formed a loop with their position targets: "
      ++ intercalate " -> "
      (Text.unpack . Path.unSeg . Path.unPlaceholder <$> targs)
    MissingCreatePositionTarget ph eps -> printf
      "Create for %s references missing position target %s"
      (show ph) (show eps)
    CEErrorString s -> s

instance Wraps AccessError ConsumerError where
  wrap = CEAccessError

instance Wraps ValidationError ConsumerError where
  wrap = CEValidationError

instance Wraps StructuralError ConsumerError where
  wrap = CEStructuralError

instance Wraps (SeqOpError EPS) ConsumerError where
  wrap = CESeqOpError

instance Wraps ErrorString ConsumerError where
  wrap = CEErrorString . unErrorString


instance MonadFail (Either ErrorString) where
  fail = Left . ErrorString

instance MonadFail (Either ValidationError) where
  fail = Left . DataValidationError


type VsM e m = ExceptT e (StateT Valuespace m)
type VsM' e m = ErrsT Valuespace (Mol DataErrorIndex e) m

castVsMError :: Monad m => DataErrorIndex -> VsM e m a -> VsM' e m a
castVsMError idx m =
  (lift $ lift $ runExceptT m) >>=
  either (throwError . Mol.singleton idx) return

castSingleErr
  :: Monad m => VsM e m a -> ErrsT Valuespace [e] m a
castSingleErr m =
  (lift $ lift $ runExceptT m) >>= either (throwError . pure) return

pathError :: Monad m => Path -> VsM e m a -> VsM' e m a
pathError p = castVsMError $ PathError p

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

note :: MonadErrors '[e1] e2 m => e1 -> Maybe a -> m a
note err = maybe (throw err) return

lookupDef
  :: (MonadState Valuespace m, MonadErrors '[AccessError] e m)
  => DefName -> m SomeDefinition
lookupDef dn = use vsTyDefs >>= note (DefNotFound dn) . Map.lookup dn

lookupPostDef
  :: (MonadState Valuespace m, MonadErrors '[AccessError] e m)
  => PostDefName -> m PostDefinition
lookupPostDef dn = use vsPostDefs >>= note (PostDefNotFound dn) . Map.lookup dn

pathTyInfo
  :: (MonadState Valuespace m, MonadErrors '[AccessError, ErrorString] e m)
  => Path -> m (DefName, Editability)
pathTyInfo path = do
    dn <- use vsRootDefName
    ed <- use vsRootEditability
    go path (dn, ed)
  where
    go
      :: (MonadState Valuespace m, MonadErrors '[AccessError, ErrorString] e m)
      => Path -> (DefName, Editability) -> m (DefName, Editability)
    go (s :</ p) (dn, _) = do
      SomeDefinition def <- lookupDef dn
      r <- liftEither @ErrorString $ getTyInfoForSeg s def
      go p r
    go _ r = return r

pathDefName
  :: (MonadState Valuespace m, MonadErrors '[AccessError, ErrorString] e m)
  => Path -> m DefName
pathDefName path = fst <$> pathTyInfo path

pathEditability
  :: (MonadState Valuespace m, MonadErrors '[AccessError, ErrorString] e m)
  => Path -> m Editability
pathEditability path = snd <$> pathTyInfo path

pathDef
  :: (MonadState Valuespace m, MonadErrors '[AccessError, ErrorString] e m)
  => Path -> m SomeDefinition
pathDef path = pathDefName path >>= lookupDef

pathPostDef
  :: ( MonadState Valuespace m
     , MonadErrors '[AccessError, ConsumerError, ErrorString] e m)
  => Path -> m PostDefinition
pathPostDef path = do
  SomeDefinition def <- pathDef path
  case def of
    ArrayDef { arrDefPostTy = mpd } -> maybe
      (throw CreatesNotSupported) lookupPostDef mpd
    -- FIXME: Might be better to have have a more specific error type here...
    _ -> throw CreatesNotSupported

pathNode
  :: (MonadState Valuespace m, MonadErrors '[AccessError] e m)
  => Path -> m (RoseTreeNode [SomeWireValue])
pathNode path = use vsTree >>= guard . Tree.lookupNode path
  where
    guard Nothing = throw NodeNotFound
    guard (Just n) = case n of
      -- FIXME: More evidence for getting rid of RtnEmpty?
      RtnEmpty -> throw NodeNotFound
      _ -> return n

pathExists :: MonadState Valuespace m => Path -> m Bool
pathExists path = use vsTree >>= return . go . Tree.lookupNode path
  where
    go Nothing = False
    go (Just RtnEmpty) = False
    go _ = True

pathChildren
  :: (MonadState Valuespace m, MonadErrors '[AccessError] e m)
  => Path -> m [Seg]
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
    _ <- collect $ AL.alFmapWithKey updatePathData $ trpdData trpd
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

-- FIXME: Once we've got everything working, we should probably re-arrange the
-- module structure so that common helpers are in their own modules and we have
-- two separate modules for process* and their specific helpers:
processTrcud
  :: Monad m
  => TrcUpdateDigest -> Valuespace
  -> m (Mol DataErrorIndex Text, FrpDigest)
processTrcud trcud vs = first (fmap errText) . fst
  <$> Error.softRunErrsT (frpdEmpty $ trcudNs trcud) (processTrcud_ trcud) vs

processTrcud_ :: Monad m => TrcUpdateDigest -> VsM' ConsumerError m FrpDigest
processTrcud_ (Trcud ns dat crs cops) = do
  (newPhs, crs') <- guardCreates crs
  dat' <- guardClientUpdates dat
  cops' <- guardClientCops newPhs cops
  return $ Frpd ns dat' crs' cops'

type PathCreates = Map Placeholder (Maybe Attributee, CreateOp)

guardCreates
  :: ( Errs '[AccessError, ConsumerError, ErrorString, ValidationError] e
     , Monad m)
  => Creates -> VsM' e m (Mos Path Placeholder, Creates)
guardCreates = fmap output . Error.filterErrs . Map.mapWithKey perPath
  where
    perPath
      :: ( Errs '[AccessError, ConsumerError, ErrorString, ValidationError] e
         , Monad m)
      => Path -> PathCreates -> VsM' e m PathCreates
    perPath p m = do
      pd <- pathError p $ pathPostDef p
      pathErrors p $ validateCreates p pd m

    output :: Map Path PathCreates -> (Mos Path Placeholder, Creates)
    output m = (Mos.fromMap $ Map.keysSet <$> m, m)

guardReadOnly
  :: (Errs '[roErr, ErrorString, AccessError] e, Monad m)
  => roErr -> Path -> VsM' e m ()
guardReadOnly roErr p = pathError p $ pathEditability p >>= \case
  ReadOnly -> throw roErr
  Editable -> return ()

guardClientUpdates
  :: ( Errs
       '[ AccessError, ConsumerError, ErrorString, StructuralError
        , ValidationError] e
     , Monad m)
  => DataDigest -> VsM' e m DataDigest
guardClientUpdates = Error.filterErrs . AL.alFmapWithKey atPath
  where
    atPath
      :: ( Errs
           '[ AccessError, ConsumerError, ErrorString, StructuralError
            , ValidationError] e
         , Monad m)
      => Path -> DataChange -> VsM' e m DataChange
    atPath p dc = do
      guardReadOnly ReadOnlyEdit p
      exists <- pathError p $ pathExists p
      if exists
        then updatePathData p dc
        else pathError p $ throw NodeNotFound
      return dc

type EPS = Either Placeholder Seg

guardClientCops
  :: ( Errs
       '[ SeqOpError EPS, ErrorString, AccessError, ConsumerError
        , StructuralError] e
     , Monad m)
  => Mos Path Placeholder -> ContOps EPS -> VsM' e m (ContOps EPS)
guardClientCops pphs = Error.filterErrs . Map.mapWithKey perPath
  where
    perPath
      :: (Errs
          '[ ErrorString, AccessError, ConsumerError, SeqOpError EPS
           , StructuralError] e
         , Monad m)
      => Path -> Map Seg (x, SequenceOp EPS)
      -> VsM' e m (Map Seg (x, SequenceOp EPS))
    perPath p m = do
      guardReadOnly ReadOnlySeqOps p
      SomeDefinition def <- pathError p $ pathDef p
      case def of
        ArrayDef {} -> do
          kids <- pathError p $ Set.fromList <$> pathChildren p
          pathErrors p $ doFilter (validateCop kids $ Mos.lookup p pphs) m
        _ -> pathError p $ throw $ SeqOpsOnNonArray

    validateCop
      :: (Wraps (SeqOpError EPS) e, MonadError [e] m)
      => Set Seg -> Set Placeholder -> Seg -> (x, SequenceOp EPS) -> m ()
    validateCop kids phs kidToChange (_, so) =
      case so of
        SoAfter (Just t) -> do
          unless (kidToChange `Set.member` kids) $
            throwError $ wrap <$> [SeqOpMovedMissingChild @EPS kidToChange]
          unless (either (`Set.member` phs) (`Set.member` kids) t) $
            throwError $ wrap <$> [SeqOpTargetMissing @EPS kidToChange t]
        -- It doesn't matter if the client removed something that's already
        -- gone:
        _ -> return ()


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


updatePathData
  :: ( Errs '[StructuralError, ValidationError, ErrorString, AccessError] e
     , Monad m)
  => Path -> DataChange -> VsM' e m ()
updatePathData p dc = do
    SomeDefinition def <- pathError p $ pathDef p
    case def of
      TupleDef {tupDefILimit = ilimit } -> case ilimit of
        Nothing -> applyConstChange def dc
        _ -> applyTsChanges def dc
      _ -> pathError p $ throw UnexpectedNodeType
  where
    applyConstChange
      :: ( Errs '[StructuralError, ValidationError, ErrorString, AccessError] e
         , Monad m)
      => Definition 'Tuple -> DataChange -> VsM' e m ()
    applyConstChange tdef = \case
      TimeChange {} -> pathError p $ throw TsChangeOnConst
      ConstChange att wvs -> pathErrors p $ do
        tyAsserts <- validateTupleValues tdef wvs
        modifying vsTree $ Tree.constSetAt att p wvs
        modifying vsTac $ Vs2Xrefs.updateConstTas (Vs2Xrefs.Referer p) tyAsserts

    applyTsChanges
      :: ( Errs '[AccessError, ErrorString, StructuralError, ValidationError] e
         , Monad m)
      => Definition 'Tuple -> DataChange -> VsM' e m ()
    applyTsChanges tdef = \case
      TimeChange m -> void $ collect $ Map.mapWithKey (applyTpChange tdef) m
      ConstChange {} -> pathError p $ throw ConstChangeOnTs

    applyTpChange
      :: (Errs '[AccessError, ErrorString, ValidationError] e, Monad m)
      => Definition 'Tuple -> TpId -> (Maybe Attributee, TimeSeriesDataOp)
      -> VsM' e m ()
    applyTpChange tdef tpid (att, tdOp) = tpErrors p tpid $ case tdOp of
      OpSet t wvs i -> do
        unless (Just (typeEnumOf i) <= tupDefILimit tdef) $ throwError $ wrap
          <$> [BadInterpolationType (typeEnumOf i) (tupDefILimit tdef)]
        tyAsserts <- validateTupleValues tdef wvs
        eitherModifying vsTree $
          first (\s -> [wrap $ ErrorString s]) . Tree.setTpAt att p tpid t wvs i
        modifying vsTac $
          Vs2Xrefs.updateTpTas (Vs2Xrefs.Referer p) tpid tyAsserts
      OpRemove -> do
        eitherModifying vsTree $
          first (\s -> [wrap $ ErrorString s]) . Tree.removeTpAt att p tpid
        modifying vsTac $ Vs2Xrefs.removeTpTas (Vs2Xrefs.Referer p) tpid

updateContainer
  :: (Errs '[AccessError, ErrorString, StructuralError] e, Monad m)
  => Path -> Map Seg (Maybe Attributee, SequenceOp Seg) -> VsM' e m ()
updateContainer p cOps = pathError p $ do
  SomeDefinition def <- pathDef p
  case def of
    ArrayDef {} -> eitherModifying vsTree
      $ first (wrap . ErrorString) . Tree.applyReorderingsAt p cOps
    -- FIXME: Potentially better error value here?
    _ -> throw SeqOpsOnNonArray

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
      => Path -> Map Seg (DefName, Editability)
      -> Map Seg (DefName, Editability) -> VsM' e m (Map Path TypeImpl)
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
          pathError p $ throw $ RemovedWhileReferencedBy $
            Vs2Xrefs.referers (Vs2Xrefs.Referee p) tac
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
          (Nothing,) <$> fullOrderOps (AL.alKeys_ tyInfo)
      ArrayDef {} -> modifying vsTree $ Tree.initContainerAt p


validateTupleValues
  :: (Errs '[ValidationError, ErrorString, AccessError] e, Monad m)
  => Definition 'Tuple -> [SomeWireValue]
  -> ErrsT Valuespace [e] m Vs2Xrefs.TypeAssertions
validateTupleValues tdef wvs = do
  tas <- eitherThrow
    $ first (fmap $ wrap . DataValidationError)
    $ validateValues (toList $ tupDefTys tdef) wvs
  checkTypeAssertions tas

validateCreateValues
  :: (Errs '[AccessError, ErrorString, ValidationError] e, Monad m)
  => PostDefinition -> CreateOp -> ErrsT Valuespace [e] m ()
validateCreateValues pdef cr = do
    tas <- eitherThrow $ first (fmap $ wrap . DataValidationError) $ combine
      $ fmtStrictZipError "post def arg types" "list of wire values"
      $ strictZipWith validateValues (toList $ postDefArgs pdef) (ocArgs cr)
    void $ checkTypeAssertions tas
  where
    collectErrors
      -- FIXME: I feel like I've written this kind of utility before...
      :: [Either [String] (Set TypeAssertion)]
      -> Either [String] (Set TypeAssertion)
    collectErrors x =
      let
        (errs, tas) = foldMap (either (,mempty) (mempty,)) x
      in
        if null errs then Right tas else Left errs

    combine
      :: Either String [Either [String] (Set TypeAssertion)]
      -> Either [String] (Set TypeAssertion)
    combine = join . bimap pure collectErrors

validateCreates
  :: ( Errs '[AccessError, ConsumerError, ErrorString, ValidationError] e
     , Monad m)
  => Path -> PostDefinition -> PathCreates -> ErrsT Valuespace [e] m PathCreates
validateCreates p pdef pCrs = do
    pCrsWithValidVals <-
      doFilter (\_ph (_att, cr) -> validateCreateValues pdef cr) pCrs
    kids <- castSingleErr $ Set.fromList <$> pathChildren p
    sortOutAfterDeps kids pCrsWithValidVals

doFilter
    :: (Monoid e, Monad m)
    => (k -> a -> ErrsT s e m b) -> Map k a -> ErrsT s e m (Map k a)
doFilter f = Error.filterErrs . Map.mapWithKey (\k a -> f k a >> return a)

-- FIXME: Rename?
sortOutAfterDeps
  :: (Errs '[ConsumerError] e, Monad m)
  => Set Seg -> PathCreates -> ErrsT s [e] m PathCreates
sortOutAfterDeps existingKids =
      filterDuplicateTargets >=> filterCycles >=> filterMissingNames
  where
    filterDuplicateTargets
      :: (Errs '[ConsumerError] e, MonadWriter [e] m)
      => PathCreates -> m PathCreates
    filterDuplicateTargets pCrs =
      let
        duplicates :: Map (Maybe EPS) (Set Placeholder)
        duplicates = Map.filter ((> 1) . Set.size) $ unMos $
          Mos.invertMap $ ocAfter . snd <$> pCrs
      in do
        _ <- sequence $ Map.mapWithKey
          (\targ phs -> tell [wrap $ MultipleCreatesReferencedTarget phs targ])
          duplicates
        return $ pCrs `Map.withoutKeys` fold duplicates

    filterCycles
      :: (Errs '[ConsumerError] e, MonadWriter [e] m)
      => PathCreates -> m PathCreates
    filterCycles pCrs =
      let
        badEdges = detectCycles . Set.fromList
          . Map.toList . lefts . justs $ ocAfter . snd <$> pCrs
        cycles = reverse
          . (\(from, _to) -> followCycle [from] from from)
          <$> badEdges
        followCycle acc ph stopAt = case Map.lookup ph pCrs of
          Just (_, OpCreate _ (Just (Left ph'))) ->
            if ph' == stopAt then acc else followCycle (ph' : acc) ph' stopAt
          -- Only PHs should be able to form cycles, so we should never hit this
          -- case:
          _ -> acc
        badPhs = mconcat $ Set.fromList <$> cycles
      in do
        mapM_ (tell . pure . wrap . CyclicReferencesInCreates) cycles
        return $ pCrs `Map.withoutKeys` badPhs

    partitionOnAfter
      :: (Maybe EPS -> Bool) -> PathCreates -> (PathCreates, PathCreates)
    partitionOnAfter f = Map.partition (f . ocAfter . snd)

    filterMissingNames
      :: (Errs '[ConsumerError] e, MonadWriter [e] m)
      => PathCreates -> m PathCreates
    filterMissingNames pCrs =
      let
        (roots, dependants) = partitionOnAfter
          (maybe True (either (`Map.notMember` pCrs) (const True))) pCrs
        (validRoots, invalidRoots) = partitionOnAfter
          (maybe True (either (const False) (`Set.member` existingKids))) roots
      in do
        _ <- sequence $ Map.mapWithKey
          (\ph (_, crop) -> tell $ maybe
            []  -- `Nothing` is always a valid root
            (pure . wrap . MissingCreatePositionTarget ph) $
            ocAfter crop)
          invalidRoots
        return $ validRoots <> dependants

-- | Returns a list of arbitrary edges from the set that if removed will break
--   cycles
detectCycles :: Ord a => Set (a, a) -> [(a, a)]
detectCycles edges =
  let
    nodes = Set.mapMonotonic fst edges <> Set.map snd edges
  in
    snd $ foldl' go (Set.map Set.singleton nodes, []) edges
  where
    go :: Ord a => (Set (Set a), [(a, a)]) -> (a, a) -> (Set (Set a), [(a, a)])
    go (equivalenceSets, cycles) edge@(from, to) =
      let
        (matches, others) = Set.partition
          (\as -> from `Set.member` as || to `Set.member` as)
          equivalenceSets
      in
        case Set.size matches of
          -- We found a cycle, because `from` was already connected to `to`:
          1 -> (equivalenceSets, edge : cycles)
          -- We union the sets of connected nodes:
          2 -> (Set.singleton (fold matches) <> others, cycles)
          -- This should not happen, but is not harmful:
          _n -> (equivalenceSets, cycles)


checkTypeAssertions
  :: (Errs '[ValidationError, ErrorString, AccessError] e, Monad m)
  => Set TypeAssertion
  -> ErrsT Valuespace [e] m Vs2Xrefs.TypeAssertions
checkTypeAssertions tas = mapM_ checkTypeAssertion tas
  >> return (Vs2Xrefs.toTypeAssertions tas)

-- FIXME: naming is confusing between this one that grabs the actDn and the
-- actual check which takes actDn:
checkTypeAssertion
  :: (Errs '[ValidationError, ErrorString, AccessError] e, Monad m)
  => TypeAssertion -> ErrsT Valuespace [e] m ()
checkTypeAssertion (TypeAssertion path expDn) = castSingleErr $ do
    actDn <- pathDefName path
    unless (actDn == expDn) $ throw $ XRefError expDn actDn

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
    modifying vsTac $ Vs2Xrefs.updateConstTas (Vs2Xrefs.Referer p) tyAsserts
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
      modifying vsTac $ Vs2Xrefs.updateTpTas (Vs2Xrefs.Referer p) tpid tyAsserts


-- FIXME: these are named incorrectly and should be something to do with
-- checking type assertions without clashing with anything checkTypeAssertiony
-- above!
revalidateXrefs
  :: (Errs '[ValidationError] e, Monad m)
  => DefName -> Map Vs2Xrefs.Referer TypeAssertion -> VsM' e m ()
revalidateXrefs actDn = void . collect . Map.mapWithKey (revalidateXref actDn)

revalidateXref
  :: (Errs '[ValidationError] e, Monad m)
  => DefName -> Vs2Xrefs.Referer -> TypeAssertion -> VsM' e m ()
revalidateXref actDn referer (TypeAssertion _ expDn) =
  pathError (Vs2Xrefs.unReferer referer) $
    unless (actDn == expDn) $ throw $ XRefError expDn actDn

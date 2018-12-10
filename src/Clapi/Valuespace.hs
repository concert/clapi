{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , LambdaCase
  , OverloadedStrings
  , PartialTypeSignatures
  , RankNTypes
  , TypeFamilies
  , TypeSynonymInstances
#-}

module Clapi.Valuespace
  ( Valuespace, vsTree, vsTyDefs, vsPostDefs
  , baseValuespace
  , VsLookupDef(..), valuespaceGet, getEditable, vsLookupNode
  , processTrpd, processTrcud
  -- , validateVs
  -- , ValidationErr(..)
  , ProtoFrpDigest(..)
  ) where

import Prelude hiding (fail)
import Control.Lens (assign, modifying, use, view, _1, _2, _3)
import qualified Control.Lens as Lens
import Control.Monad (unless, liftM2, join, void)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.State
  ( State(..), StateT(..), MonadState(..), get, put, execState, evalStateT
  , modify)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Except
  (MonadError(..), ExceptT(..), runExceptT, liftEither, withExceptT)
import Control.Monad.Writer (listen)
import Data.Bifunctor (first, bimap)
import Data.Either (lefts, isRight)
import Data.Foldable (toList, fold)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Merge.Strict
  ( merge, mergeA, preserveMissing, dropMissing, mapMissing, mapMaybeMissing
  , traverseMissing, zipWithMatched, zipWithMaybeMatched, zipWithAMatched)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tagged (Tagged, untag)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word
import Text.Printf (printf)

import Data.Map.Mol (Mol(..))
import qualified Data.Map.Mol as Mol
import Data.Map.Mos (Mos(..))
import qualified Data.Map.Mos as Mos
import qualified Data.Map.Dependencies as Dependencies

import Clapi.Error
import Clapi.Util
  (strictZipWith, fmtStrictZipError, Partition(..), partitionEithers', foldMapM)
import Clapi.Tree
  ( RoseTree(..), RoseTreeNode(..), treeInsert, treeChildren
  , RoseTreeNodeType(..), treePaths)
import qualified Clapi.Tree as Tree
import Clapi.Types (WireValue(..))
import Clapi.Types.Base (InterpolationLimit(ILUninterpolated), Attributee)
import Clapi.Types.AssocList
  ( AssocList, alKeysSet, alKeys, alValues, alEmpty, alTraverseWithKey
  , alFmapWithKey, alToMap, alPartitionWithKey, alFilterKey, unAssocList
  , alMapMaybe)
import Clapi.Types.Definitions
  ( Definition(..), Editable(..), Definition(..), SomeDefinition(..), DefName
  , PostDefinition(..), PostDefName, MetaType(..), childEditableFor
  , getTyInfoForSeg, structDef)
import Clapi.Types.Digests
  ( TpId, DefOp(..), isUndef, isDef, ContOps, DataChange(..), isRemove, DataDigest
  , TrpDigest(..), trpdRemovedPaths, TrcUpdateDigest(..), CreateOp(..), Creates
  , DataErrorIndex(..), TimeSeriesDataOp(..), FrcUpdateDigest(..), frcudEmpty
  , FrpDigest(..), frpdEmpty)
import Clapi.Types.Path
  ( Seg, Path, Path', pattern (:/), pattern (:</), pattern Root, Placeholder
  , childPaths)
import qualified Clapi.Types.Path as Path
import Clapi.Types.SequenceOps (SequenceOp(..), isSoAbsent)
import Clapi.Types.Tree (TreeType(..), SomeTreeType(..), SomeTreeValue(..))
import Clapi.Types.Wire (SomeWireValue(..))
import Clapi.Types.UniqList (unUniqList)
import Clapi.Validator (validateValues, revalidateValues, extractTypeAssertions)
import qualified Clapi.Types.Dkmap as Dkmap

import Clapi.Internal.Valuespace
  ( Valuespace(..)
  , vsTree, vsTyAssns, vsTyDefs, vsPostDefs, vsRootDefName, vsRootEditable
  , DefMap, TypeAssignmentMap, Referer, Referee, Xrefs)

import Debug.Trace

type VsM = ErrsM Valuespace (Mol DataErrorIndex Text)

note :: Text -> Maybe a -> ErrsM s Text a
note msg = maybe (aborts msg) return

lookupNode :: Path -> ErrsM Valuespace Text (RoseTreeNode [SomeTreeValue])
lookupNode path =
  use vsTree >>= note "Node not found" . Tree.treeLookupNode path

lookupChildren :: Path -> ErrsM Valuespace Text [Seg]
lookupChildren path = lookupNode path >>= return . \case
  RtnChildren al -> unUniqList $ alKeys al
  _ -> mempty

lookupDef :: DefName -> ErrsM Valuespace Text SomeDefinition
lookupDef defName = use vsTyDefs >>= note msg . Map.lookup defName
  where
    msg = Text.pack $ printf "Definition %s not found" $ show $ untag $ defName

lookupPostDef :: PostDefName -> ErrsM Valuespace Text PostDefinition
lookupPostDef postDefName = use vsPostDefs >>= note msg . Map.lookup postDefName
  where
    msg = Text.pack $ printf "Post definition %s not found" $
      show $ untag $ postDefName

-- | Lookup the definition name and editability of the path _using the current
--   map of names to definitions
pathTyInfo :: Path -> ErrsM Valuespace Text (DefName, Editable)
pathTyInfo path = do
    dn <- use vsRootDefName
    ed <- use vsRootEditable
    go path (dn, ed)
  where
    go
      :: Path -> (DefName, Editable)
      -> ErrsM Valuespace Text (DefName, Editable)
    go (s :</ p) (dn, _) = do
      SomeDefinition def <- lookupDef dn
      r <- abortsEither $ getTyInfoForSeg s def
      go p r
    go _ r = return r

pathDef :: Path -> ErrsM Valuespace Text SomeDefinition
pathDef path = pathTyInfo path >>= lookupDef . fst

pathEditable :: Path -> ErrsM Valuespace Text Editable
pathEditable path = snd <$> pathTyInfo path

pathPostDef :: Path -> ErrsM Valuespace Text PostDefinition
pathPostDef path = do
  SomeDefinition def <- pathDef path
  case def of
    ArrayDef { arrDefPostTy = mpd } -> maybe
      (aborts "Array does not support creates") lookupPostDef mpd
    _ -> aborts "Expected array definition"


-- removeTamSubtree :: Path -> TypeAssignmentMap -> TypeAssignmentMap
-- removeTamSubtree p = Dependencies.filterKey (not . flip Path.isChildOf p)

-- removeXrefs :: Referer -> Xrefs -> Xrefs
-- removeXrefs referer = fmap (Map.delete referer)

-- removeXrefsTps :: Referer -> Set TpId -> Xrefs -> Xrefs
-- removeXrefsTps referer tpids = fmap (Map.update updateTpMap referer)
--   where
--     updateTpMap Nothing = Just Nothing
--     updateTpMap (Just tpSet) = let tpSet' = Set.difference tpSet tpids in
--       if null tpSet' then Nothing else Just $ Just tpSet'

baseValuespace :: DefName -> Editable -> Valuespace
baseValuespace rootDefName rootEditable = Valuespace
    (Tree.RtContainer alEmpty)
    mempty
    (Map.singleton rootDefName emptyStructDef)
    rootDefName
    rootEditable
    (Dependencies.singleton Root rootDefName)
    mempty
  where
    emptyStructDef = structDef "Empty namespace" alEmpty


-- FIXME: Might want to rework this class, and the ErrsM variants above to be
-- more similar:
class VsLookupDef def where
  ldErrStr :: Proxy def -> String

  vsGetDefMap :: Valuespace -> DefMap def

  lookupDef' :: MonadFail m => Tagged def Seg -> DefMap def -> m def
  lookupDef' s defs = maybe (fail $ "Missing " ++ ldErrStr (Proxy @def)) return $
    Map.lookup s defs

  vsLookupDef :: MonadFail m => Tagged def Seg -> Valuespace -> m def
  vsLookupDef s = lookupDef' s . vsGetDefMap

  defForPath :: MonadFail m => Path -> Valuespace -> m def

instance VsLookupDef PostDefinition where
  ldErrStr _ = "post def"
  vsGetDefMap = view vsPostDefs
  defForPath p vs = do
    SomeDefinition def <- defForPath @SomeDefinition p vs
    case def of
      ArrayDef { arrDefPostTy = mptn } -> maybe
        (fail "array does not define post type")
        (flip vsLookupDef vs)
        mptn
      _ -> fail "Definition at path not for array"

instance VsLookupDef SomeDefinition where
  ldErrStr _ = "def"
  vsGetDefMap = view vsTyDefs
  defForPath p vs = lookupTypeName p (view vsTyAssns vs)
    >>= flip lookupDef' (view vsTyDefs vs)

lookupTypeName
  :: MonadFail m => Path -> TypeAssignmentMap -> m DefName
lookupTypeName p tam = maybe (fail "No type name found") return $
  Dependencies.lookup p tam

vsLookupNode
  :: MonadFail m => Path -> Valuespace -> m (RoseTreeNode [SomeTreeValue])
vsLookupNode path =
  maybe (fail "Node not found") return . Tree.treeLookupNode path . view vsTree

getEditable :: MonadFail m => Path -> Valuespace -> m Editable
getEditable path vs = case path of
  p :/ s ->
    defForPath p vs >>= (\(SomeDefinition def) -> childEditableFor s def)
  _ -> pure $ view vsRootEditable vs  -- Root

valuespaceGet
  :: MonadFail m => Path -> Valuespace
  -> m ( SomeDefinition
       , DefName
       , Editable
       , RoseTreeNode [SomeTreeValue])
-- valuespaceGet p vs = do
--     rtn <- vsLookupNode p vs
--     ts <- lookupTypeName p $ view vsTyAssns vs
--     def <- lookupDef' ts $ view vsTyDefs vs
--     ed <- getEditable p vs
--     return (def, ts, ed, rtn)
valuespaceGet p vs = let (ea, _) = runErrsM f vs in
    either (fail . Text.unpack) return ea
  where
    f = do
      (dn, ed) <- pathTyInfo p
      def <- lookupDef dn
      rtn <- lookupNode p
      return (def, dn, ed, rtn)

-- type RefTypeClaims = Mos DefName Referee
-- type TypeClaimsByPath =
--   Map Referer (Either RefTypeClaims (Map TpId RefTypeClaims))

-- partitionXrefs :: Xrefs -> TypeClaimsByPath -> (Xrefs, Xrefs)
-- partitionXrefs oldXrefs claims = (preExistingXrefs, newXrefs)
--   where
--     newXrefs = Map.foldlWithKey asXref mempty claims
--     asXref acc referer claimEither = case claimEither of
--       Left rtcs -> foldl (addReferer referer) acc $ Mos.valueSet rtcs
--       Right rtcm -> Map.foldlWithKey (addRefererWithTpid referer) acc rtcm
--     addReferer referer acc referee = Map.alter
--       (Just . Map.insert referer Nothing . maybe mempty id) referee acc
--     addRefererWithTpid referer acc tpid rtcs =
--       foldl (addRefererWithTpid' referer tpid) acc $ Mos.valueSet rtcs
--     addRefererWithTpid' referer tpid acc referee = Map.alter
--       (Just . Map.alter
--         (Just . Just . maybe (Set.singleton tpid) (Set.insert tpid) .
--           maybe Nothing id)
--         referer . maybe mempty id)
--       referee
--       acc
--     preExistingXrefs = Map.foldlWithKey removeOldXrefs oldXrefs claims
--     removeOldXrefs acc referrer claimEither = case claimEither of
--       Left _ -> removeXrefs referrer acc
--       Right rtcm -> removeXrefsTps referrer (Map.keysSet rtcm) acc

-- xrefUnion :: Xrefs -> Xrefs -> Xrefs
-- xrefUnion = Map.unionWith $ Map.unionWith $ liftM2 Set.union

-- checkRefClaims
--   :: TypeAssignmentMap
--   -> Map Path (Either RefTypeClaims (Map TpId RefTypeClaims))
--   -> Either (Map DataErrorIndex [ValidationErr]) ()
-- checkRefClaims tyAssns = smashErrMap . Map.mapWithKey checkRefsAtPath
--   where
--     errIf m = unless (null m) $ Left m
--     smashErrMap = errIf . unMol . Mol.unions . fmap Mol . lefts . Map.elems
--     checkRefsAtPath
--       :: Path
--       -> Either RefTypeClaims (Map TpId RefTypeClaims)
--       -> Either (Map DataErrorIndex [ValidationErr]) ()
--     checkRefsAtPath path refClaims =
--       let
--         doCheck eidx = first (Map.singleton eidx . pure @[]) .
--           mapM_ (uncurry checkRef) . Mos.toList
--       in
--         either
--           (doCheck $ PathError path)
--           (smashErrMap .
--            Map.mapWithKey (\tpid -> doCheck (TimePointError path tpid)))
--           refClaims
--     checkRef :: DefName -> Path -> Either ValidationErr ()
--     checkRef requiredTs refP = case lookupTypeName refP tyAssns of
--         Nothing -> Left $ RefTargetNotFound refP
--         Just actualTs -> if actualTs == requiredTs
--             then Right ()
--             else Left $ RefTargetTypeErr refP actualTs requiredTs

-- validateVs
--   :: Map Path (Maybe (Set TpId)) -> Valuespace
--   -> Either
--        (Map DataErrorIndex [ValidationErr])
--        (Map Path DefName, Valuespace)
-- validateVs t v = do
--     (newTypeAssns, refClaims, vs) <- inner mempty mempty t v
--     checkRefClaims (vsTyAssns vs) refClaims
--     let (preExistingXrefs, newXrefs) = partitionXrefs (vsXrefs vs) refClaims
--     let existingXrefErrs = validateExistingXrefs preExistingXrefs newTypeAssns
--     unless (null existingXrefErrs) $ Left existingXrefErrs
--     let vs' = vs {vsXrefs = xrefUnion preExistingXrefs newXrefs}
--     return (newTypeAssns, vs')
--   where
--     errP p = first (Map.singleton (PathError p) . pure . GenericErr)
--     tLook p = maybe (Left $ Map.singleton (PathError p) [ProgrammingErr "Missing RTN"]) Right .
--       Tree.treeLookup p
--     changed :: Eq a => a -> a -> Maybe a
--     changed a1 a2 | a1 == a2 = Nothing
--                   | otherwise = Just a2

--     -- FIXME: this currently bails earlier than it could in light of validation
--     -- errors. If we can't figure out the type of children in order to recurse,
--     -- then we should probably stop, but if we just encouter bad data, we should
--     -- probably just capture the error and continue.
--     inner
--       :: Map Path DefName
--       -> TypeClaimsByPath
--       -> Map Path (Maybe (Set TpId)) -> Valuespace
--       -> Either (Map DataErrorIndex [ValidationErr])
--            (Map Path DefName, TypeClaimsByPath, Valuespace)
--     inner newTas newRefClaims tainted vs =
--       let tree = vsTree vs; oldTyAssns = vsTyAssns vs in
--       case Map.toAscList tainted of
--         [] -> return (newTas, newRefClaims, vs)
--         ((path, invalidatedTps):_) ->
--           case lookupTypeName path (vsTyAssns vs) of
--             -- When we don't have the type (and haven't bailed out) we know the
--             -- parent was implictly added by the rose tree and thus doesn't
--             -- appear in the taints, but because it was changed it should be
--             -- counted as such.
--             Nothing -> case path of
--               (parentPath :/ _) -> inner
--                 newTas newRefClaims (Map.insert parentPath Nothing tainted)
--                 vs
--               _ -> Left $ Map.singleton GlobalError
--                 [GenericErr "Attempted to taint parent of root"]
--             Just ts -> do
--               def <- errP path $ vsLookupDef ts vs
--               rtn <- tLook path tree
--               case validateRoseTreeNode def rtn invalidatedTps of
--                 Left validationErrs -> if null emptyArrays
--                     then Left $ Map.singleton (PathError path) validationErrs
--                     else inner newTas' newRefClaims tainted' vs'
--                   where
--                     emptyArrays = mapMaybe mHandlable validationErrs
--                     isEmptyContainer d = case d of
--                         ArrayDef _ -> True
--                         StructDef _ defKids -> defKids == alEmpty
--                         _ -> False
--                     mHandlable ve = case ve of
--                         MissingChild name -> do
--                           cts <- defDispatch (childTypeFor name) def
--                           cdef <- vsLookupDef cts vs
--                           if isEmptyContainer cdef
--                             then Just (path :/ name, cts)
--                             else Nothing
--                         BadNodeType _ treeType ->
--                           case (treeType, path, isEmptyContainer def) of
--                             (RtntEmpty, Root, True) -> Just (Root, ts)
--                             _ -> Nothing
--                         _ -> Nothing
--                     newTas' = newTas <> Map.fromList emptyArrays
--                     tainted' = Map.fromList (fmap (const Nothing) <$> emptyArrays) <> tainted
--                     att = Nothing  -- FIXME: who is this attributed to?
--                     insertEmpty p = treeInsert att p (RtContainer alEmpty)
--                     vs' = vs {vsTree = foldl (\acc (p, _) -> insertEmpty p acc) tree emptyArrays}
--                 Right pathRefClaims -> inner
--                       (newTas <> changedChildPaths)
--                       (Map.insert path pathRefClaims newRefClaims)
--                       (Map.delete path $
--                          tainted <> fmap (const Nothing) changedChildPaths)
--                       (vs {vsTyAssns = Dependencies.setDependencies changedChildPaths oldTyAssns})
--                   where
--                     oldChildTypes = Map.mapMaybe id $ alToMap $ alFmapWithKey
--                       (\name _ -> Dependencies.lookup (path :/ name) oldTyAssns) $
--                       treeChildren rtn
--                     newChildTypes = Map.mapMaybe id $ alToMap $ alFmapWithKey
--                       (\name _ -> defDispatch (childTypeFor name) def) $
--                       treeChildren rtn
--                     changedChildTypes = merge
--                       dropMissing preserveMissing
--                       (zipWithMaybeMatched $ const changed)
--                       oldChildTypes newChildTypes
--                     changedChildPaths = Map.mapKeys (path :/) changedChildTypes

-- opsTouched :: ContOps Seg -> DataDigest -> Map Path (Maybe (Set TpId))
-- opsTouched cops dd = fmap (const Nothing) cops <> fmap classifyDc (alToMap dd)
--   where
--     classifyDc :: DataChange -> Maybe (Set TpId)
--     classifyDc (ConstChange {}) = Nothing
--     classifyDc (TimeChange m) = Just $ Map.keysSet m

updateNsDefs
  :: Map (Tagged def Seg) (DefOp def)
  -> Map (Tagged def Seg) def
  -> Map (Tagged def Seg) def
updateNsDefs defOps existingDefs =
    Map.union newDefs $ Map.withoutKeys existingDefs $ Map.keysSet unDefs
  where
    (unDefs, defs) = Map.partition isUndef defOps
    newDefs = odDef <$> defs

{- Vague recipe based on the premise that the whole tree structure is defined,
   _from the root type down_ by the types:
    * Partition the definition operations into definitions and undefinitions,
      and update the `DefName -> Definition` mapping appropriately.
    * The data updates can now be handled just by normal lookups in the new
      type mapping.
      (NB: This is potentially inefficient; if the path has changed type and
      we've changed all the values, we'll validate all the values as we insert
      them into the tree and then revalidate them all when we come to check
      the type change.)
    * Partition the definitions into new definitions and redefinitions by
      referencing the old definition mapping.
    + Use the old type assignment map to lookup the paths that were depending
      on the redefined types, thus producing a `Path -> DefName` map (`Path ->
      Definition?`)

    * Use `prefixes` to find the top-level paths of the independent subtrees
      that were affected by the redefinitions.
    * These independent paths are the paths at which we will start validation.
    + Look at each revalidation path in turn
    + Compare the definitions of that path now with its previous
      definition and determine which children have changed definition name
      changed definition (by looking at the defintion
      changes in the bundle), or have gone away. The changed definition names
      or definitions need revalidating and the removals need actually removing
      from the tree.
    + Update the state to remove the paths we've handled and add the new paths
      to inspect. By definition these new paths will be longer than the paths
      we started with, so we are guaranteed to make progress.
    + Once we have no paths left revalidate, we are done.

   WRT validating cross references:
    + Type name assertions can be extracted from all new cross references
      added in the data bundle and checked directly against the new types.
    + As part of revalidating an old path, we can check cross references
      against new types. (That checks the case where a definition changes the
      target type of a cross reference.)
    + Finally, we have to check that the targets of existing untouched cross
      references have not unexpectedly changed type.
       + We know all the paths that we revalidated (i.e. all the paths that
         changed type).
       + Looking up all of these in the cross reference cache will give us the
         biggest set of cross references we'd have to go and check.
       + However, we should have updated any that needed changing, which we
         can tell by looking at the data changes in our bundle. (Also, the
         cross reference cache for those cross references will now be invalid)
       + So, if we subtract the cross references we have changed from the
         cross references we need to have changed, we should be left with the
         ones that _are now incorrect_.
       + Probably.
-}

processTrpd
  :: TrpDigest -> Valuespace -> Either Errs (FrcUpdateDigest, Valuespace)
processTrpd trpd vs = case runErrsM (processTrpd_ trpd) vs of
    (Left errs, _) -> Left errs
    (Right frcud, vs') -> Right (frcud, vs')

processTrpd_ :: TrpDigest -> VsM FrcUpdateDigest
processTrpd_ trpd =
  let
    (pTyDefs, pTyUndefs) = partitionDefOps $ trpdPostDefs trpd
  in do
    defChanges <- classifyDefOps (trpdDefinitions trpd) <$> use vsTyDefs
    let (tyNewDefs, tyRedefs, tyUndefs) = partitionDefChanges defChanges
    oldTyDefs <- use vsTyDefs
    oldTree <- use vsTree

    -- Now we know which types have been redefined vs newly defined, we can
    -- safely go ahead and update the `DefName -> Definition` mapping:
    modifying vsTyDefs
      $ Map.union tyNewDefs
      . Map.union (snd <$> tyRedefs)
      . flip Map.withoutKeys tyUndefs

    -- Lets update the rest of the primary state:
    updateDefs vsPostDefs pTyDefs pTyUndefs
    collect $ alFmapWithKey updatePathData $ trpdData trpd
    -- FIXME: The container updates might need to happen later, if changing the
    -- types has a material effect on what the types of the tree nodes are:
    collect $ Map.mapWithKey updateContainer $ trpdContOps trpd

    roughImpls <- flip possibleImplications defChanges <$> use vsTyAssns
    -- Thsese are the top-level type change implications that we _know_ will
    -- need to be checked:
    let initialImpls = Path.prefixesMap roughImpls
    extendedImpls <- (initialImpls <>) <$> extendImplications oldTree initialImpls
    -- We add back any initially implications from the def changes that
    -- _haven't_ been superceded by the recursive type implications:
    let allImpls = extendedImpls <> roughImpls

    collect $ Map.mapWithKey handleImplications allImpls
    return $ generateFrcud allImpls
  where
    updateDefs lens defs undefs = modifying lens $
      Map.union defs . flip Map.withoutKeys undefs

    generateFrcud allImpls = FrcUpdateDigest
      (trpdNamespace trpd)
      (trpdPostDefs trpd)
      (trpdDefinitions trpd)
      (Map.foldMapWithKey generateTyAsn allImpls)
      (trpdData trpd)
      (trpdContOps trpd)
      (trpdErrors trpd)

    generateTyAsn
      :: Path -> ThingsWhatCanHaveHappenedToAPath
      -> Map Path (DefName, Editable)
    generateTyAsn p = \case
      -- FIXME: Do we want to track editability changes through the implications
      -- thing too?
      -- FIXME: We might be able to use the implication to minimise the the
      -- definitions broadcast out to the clients too...
      TypeReassigned _ dn _ _ ed -> Map.singleton p (dn, ed)
      NewlyAssigned dn _ ed -> Map.singleton p (dn, ed)
      _ -> mempty

-- | Partitions DefOps into those that were defining a type and those removing a
--   type.
partitionDefOps :: Map k (DefOp a) -> (Map k a, Set k)
partitionDefOps = bimap (fmap odDef) Map.keysSet . Map.partition isDef

type TypeRedefinition = (DefName, SomeDefinition, SomeDefinition)
type TypeReassignment = (DefName, DefName)

data ThingsWhatCanHaveHappenedToAPath
  = TypeRedefined DefName SomeDefinition SomeDefinition
  -- NB only new editablilities
  | TypeReassigned DefName DefName SomeDefinition SomeDefinition Editable
  | NewlyAssigned DefName SomeDefinition Editable
  | ImplicitlyRemoved
  deriving (Show)

type DefDiff =
  ( DefMap SomeDefinition
  , Map DefName (SomeDefinition, SomeDefinition)
  , Set DefName)

-- Like DefOp, but takes into account previous state by separating new
-- definitions and redefinitions:
data DefChange
  = NewDef SomeDefinition
  | Redef SomeDefinition SomeDefinition
  | Undef

classifyDefOps
  :: Map DefName (DefOp SomeDefinition) -> Map DefName SomeDefinition
  -> Map DefName DefChange
classifyDefOps defOps oldTyDefs = merge
    dropMissing (mapMaybeMissing f) (zipWithMatched g) oldTyDefs defOps
  where
    f dn = \case
      OpDefine newDef -> Just $ NewDef newDef
      OpUndefine -> Nothing
    g dn oldDef = \case
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

-- | Paritions DefOps into those that were defining a new type, those that were
--   redefining an existing type, and those that were undefining a type.
partitionDefOps'
  :: DefMap SomeDefinition -> Map DefName (DefOp SomeDefinition) -> DefDiff
partitionDefOps' oldTyDefs defOps = flip execState mempty $ mergeA
    dropMissing (mapMissing f) (zipWithMatched g) oldTyDefs defOps
  where
    f :: _ -> _ -> State DefDiff ()
    f dn = \case
      OpDefine newDef -> modifying _1 $ Map.insert dn newDef
      OpUndefine -> return ()
    g dn oldDef = \case
      OpDefine newDef -> modifying _2 $ Map.insert dn (oldDef, newDef)
      OpUndefine -> modifying _3 $ Set.insert dn

-- | Find the roots of all subtrees with changed types. For example, given the
--   following tree, where nodes with changed type are marked with ', we want to
--   find only nodes a/b and a/c/f, marked with []:
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
lookAtTheseFirst :: Map DefName a -> TypeAssignmentMap -> Set Path
lookAtTheseFirst tyRedefs tyAssns = Path.prefixes $ mconcat $
  (\dn -> Dependencies.lookupRev dn tyAssns) <$> Map.keys tyRedefs

-- | Do the path findy thing, but collate some useful type information along the
--   way
lookAtTheseFirst'
  :: Map DefName (SomeDefinition, SomeDefinition) -> TypeAssignmentMap
  -> Map Path (DefName, SomeDefinition, SomeDefinition)
lookAtTheseFirst' tyRedefs oldTyAssns = Path.prefixesMap $ Map.fromList $
    foldMap getDepInfo $ Map.toList tyRedefs
  where
    getDepInfo (dn, (oldDef, newDef)) = fmap (,(dn, oldDef, newDef)) $
        Set.toList $ Dependencies.lookupRev dn oldTyAssns

-- Look at the changes to the definitions and make rough stab at what they could
-- have done to the types and structure of the tree.
possibleImplications
  :: TypeAssignmentMap -> Map DefName DefChange
  -> Map Path ThingsWhatCanHaveHappenedToAPath
possibleImplications tyAssns = Map.foldMapWithKey f
  where
    f :: DefName -> DefChange -> Map Path ThingsWhatCanHaveHappenedToAPath
    f dn dc = case dc of
        NewDef def -> mempty
        Redef oldDef newDef -> depPs dn $ TypeRedefined dn oldDef newDef
        Undef -> depPs dn $ ImplicitlyRemoved
    depPs dn x = Map.fromSet (const x) $ Dependencies.lookupRev dn tyAssns

mangleRedef
  :: RoseTree a -> Path -> SomeDefinition -> SomeDefinition
  -> VsM (Map Path ThingsWhatCanHaveHappenedToAPath)
mangleRedef oldTree p (SomeDefinition oldDef) (SomeDefinition newDef) =
  let
    oldTyInfo = getChildTyInfoWith p oldDef oldTree
  in do
    newTyInfo <- getChildTyInfoWith p newDef <$> use vsTree
    mangleFoo p oldTyInfo newTyInfo

mangleNewDef
  :: Path -> SomeDefinition -> VsM (Map Path ThingsWhatCanHaveHappenedToAPath)
mangleNewDef p (SomeDefinition def) = do
  newTyInfo <- getChildTyInfoWith p def <$> use vsTree
  mangleFoo p mempty newTyInfo

mangleFoo
  :: Path -> Map Seg (DefName, Editable) -> Map Seg (DefName, Editable)
  -> VsM (Map Path ThingsWhatCanHaveHappenedToAPath)
mangleFoo p oldTyInfo newTyInfo =
    fold <$> mergeA (traverseMissing f) (traverseMissing g) (zipWithAMatched h)
      oldTyInfo newTyInfo
  where
    f n _ = return $ Map.singleton (p :/ n) ImplicitlyRemoved
    g n (dn, ed)= do
      def <- pathError p $ lookupDef dn
      return $ Map.singleton (p :/ n) $ NewlyAssigned dn def ed
    h n (dn1, ed1) (dn2, ed2) = do
      if dn1 == dn2 && ed1 == ed2
        then return mempty
        else do
          def1 <- pathError p $ lookupDef dn1
          def2 <- pathError p $ lookupDef dn2
          return $ Map.singleton (p :/ n) $ TypeReassigned dn1 dn2 def1 def2 ed2


-- FIXME: I think this needs to take into account the roughImpls too
extendImplications
  :: RoseTree a
  -> Map Path ThingsWhatCanHaveHappenedToAPath
  -> VsM (Map Path ThingsWhatCanHaveHappenedToAPath)
extendImplications oldTree initialImpls
  | Map.null initialImpls = return mempty
  | otherwise = do
      newImplications <-
        mapFoldMapMWithKey (flangeThingsWhat oldTree) initialImpls
      recImpls <- extendImplications oldTree newImplications
      return $ newImplications <> recImpls


-- Given a thing that happened to a path, what things happened directly below
-- it?
flangeThingsWhat :: RoseTree a -> Path -> ThingsWhatCanHaveHappenedToAPath
  -> VsM (Map Path ThingsWhatCanHaveHappenedToAPath)
flangeThingsWhat oldTree p = \case
  TypeRedefined dn def1 def2 -> mangleRedef oldTree p def1 def2
  TypeReassigned dn1 dn2 def1 def2 ed2 -> mangleRedef oldTree p def1 def2
  NewlyAssigned dn def ed -> mangleNewDef p def
  ImplicitlyRemoved -> return mempty


getChildTyInfo :: Definition mt -> RoseTreeNode a -> Map Seg (DefName, Editable)
getChildTyInfo def node = case def of
  TupleDef {} -> mempty
  StructDef { strDefChildTys = tyinfo } -> alToMap tyinfo
  ArrayDef { arrDefChildTy = dn, arrDefChildEditable = ed } -> case node of
    RtnChildren al -> const (dn, ed) <$> alToMap al
    _ -> mempty

getChildTyInfoWith
  :: Path -> Definition mt -> RoseTree a -> Map Seg (DefName, Editable)
getChildTyInfoWith p def =
  getChildTyInfo def . maybe RtnEmpty id . Tree.treeLookupNode p

handleImplications :: Path -> ThingsWhatCanHaveHappenedToAPath -> VsM ()
handleImplications p = \case
  TypeRedefined dn def1 (SomeDefinition def2) ->
    -- Revalidate the path according to def2
    revalidatePath def2 p
  TypeReassigned dn1 dn2 def1 (SomeDefinition def2) ed2 -> do
    -- Revalidate the path according to def2
    modifying vsTyAssns $ Dependencies.insert p dn2
    revalidatePath def2 p
  NewlyAssigned dn def ed -> do
    -- All the new data will have already been validated, so checking the node
    -- is not empty will suffice:
    node <- pathError p $ lookupNode p
    case node of
      RtnEmpty -> pathError p $ aborts "No data for new node"
      _ -> return ()
    modifying vsTyAssns $ Dependencies.insert p dn
  ImplicitlyRemoved -> do
    modifying vsTree $ Tree.treeDelete p
    modifying vsTyAssns $ Dependencies.delDependency p

updatePathData :: Path -> DataChange -> VsM ()
updatePathData p dc = do
  SomeDefinition def <- pathError p $ pathDef p
  case def of
    TupleDef { tupDefILimit = ilimit } -> case ilimit of
      ILUninterpolated -> applyConstChange def p dc
      _ -> applyTsChanges def p dc
    _ -> pathErrors p $ abort "Not a data node"

applyConstChange :: Definition 'Tuple -> Path -> DataChange -> VsM ()
applyConstChange tdef p = pathErrors p . \case
    TimeChange {} -> abort "Time series change on constant data node"
    ConstChange att wvs -> do
      tvs <- validateTupleValues tdef wvs
      modifying vsTree $ Tree.treeConstSetAt att p tvs

applyTsChanges :: Definition 'Tuple -> Path -> DataChange -> VsM ()
applyTsChanges tdef p = \case
  ConstChange {} ->
    pathErrors p $ abort "Constant data change on time series data node"
  TimeChange m -> void $ collect $ Map.mapWithKey (applyTpChange tdef p) m

applyTpChange
  :: Definition 'Tuple -> Path -> TpId -> (Maybe Attributee, TimeSeriesDataOp)
  -> VsM ()
applyTpChange tdef p tpid (att, tdOp) = timePointErrors p tpid $ case tdOp of
    OpSet t wvs i -> do
      tvs <- validateTupleValues tdef wvs
      modifying'' vsTree $ first pure . Tree.treeTpSetAt att p tpid t tvs i
    OpRemove ->
      modifying'' vsTree $ first pure . Tree.treeTpRemoveAt att p tpid

validateTupleValues
  :: Definition 'Tuple -> [SomeWireValue] -> ErrsM s [Text] [SomeTreeValue]
validateTupleValues tdef =
  abortsEither . validateValues (toList $ tupDefTys tdef)

updateContainer :: Path -> Map Seg (Maybe Attributee, SequenceOp Seg) -> VsM ()
updateContainer p cOps = pathError p $
  modifying'' vsTree $ Tree.treeApplyReorderingsAt p cOps

revalidatePath :: Definition mt -> Path -> VsM ()
revalidatePath def = case def of
  TupleDef {} -> revalidatePathData def
  StructDef {} -> revalidateContainerKeys def
  ArrayDef {} -> revalidateContainerKeys def

revalidateContainerKeys :: Definition mt -> Path -> VsM ()
revalidateContainerKeys def path = pathError path $ do
  node <- lookupNode path
  case node of
    RtnChildren al -> case def of
      StructDef {strDefChildTys = tyInfo} ->
        unless (alKeys tyInfo == alKeys al) $
          aborts $ Text.pack $ printf "Bad child keys %s" $ show $ alKeys al
      ArrayDef {} -> return ()
      TupleDef {} -> error "Should not call with TupleDef"
    _ -> aborts "Expected container node"


revalidatePathData :: Definition 'Tuple -> Path -> VsM ()
revalidatePathData def@(TupleDef {tupDefILimit = ilimit }) p =
    case ilimit of
      ILUninterpolated -> goRevalidate $ revalidateConstData def p
      _ -> goRevalidate $ revalidateTsData def p
  where
    goRevalidate f = modifying''' vsTree $ Tree.treeAlterF Nothing (atNode f) p
    atNode f = \case
      Just rtn -> Just <$> f rtn
      Nothing -> pathError p $ aborts "Missing node for revalidation"

revalidateConstData
  :: Definition 'Tuple -> Path -> RoseTree [SomeTreeValue]
  -> ErrsM s Errs (RoseTree [SomeTreeValue])
revalidateConstData tdef p = \case
  RtConstData att tvs ->
    pathErrors p (revalidateTupleValues tdef tvs) >>= return . RtConstData att
  _ -> pathError p $ aborts "Node defined to be constant is wrong type"

revalidateTsData
  :: Definition 'Tuple -> Path -> RoseTree [SomeTreeValue]
  -> ErrsM s Errs (RoseTree [SomeTreeValue])
revalidateTsData tdef p = \case
  RtDataSeries ts -> collect (Dkmap.mapWithKeys
    (\tpid _ tp -> traverse
      (traverse $ timePointErrors p tpid . revalidateTupleValues tdef)
      tp)
    ts) >>= return . RtDataSeries
  _ -> pathError p $ aborts "Node defined to be time series is wrong type"

revalidateTupleValues
  :: Definition 'Tuple -> [SomeTreeValue] -> ErrsM s [Text] [SomeTreeValue]
revalidateTupleValues tdef =
  abortsEither . revalidateValues (toList $ tupDefTys tdef)

mapFoldMWithKey
  :: forall k a b m. Monad m => (b -> k -> a -> m b) -> b -> Map k a -> m b
mapFoldMWithKey f b m = Map.foldrWithKey f' return m b
  where
    f' :: k -> a -> (b -> m b) -> b -> m b
    f' k a fm b' = f b' k a >>= fm

mapFoldMapMWithKey
  :: forall k a b m. (Monoid b, Monad m) => (k -> a -> m b) -> Map k a -> m b
mapFoldMapMWithKey f = mapFoldMWithKey f' mempty
  where
    f' :: b -> k -> a -> m b
    f' b k a = (b <>) <$> f k a

modifying'
  :: MonadState s m => Lens.Lens' s a -> (a -> Either x a) -> m (Either x ())
modifying' lens f = do
  state <- use lens
  case f state of
    Left s -> return $ Left s
    Right state' -> assign lens state' >> return (Right ())

-- validatePath :: Valuespace -> Path -> Maybe (Set TpId) -> Either [ValidationErr] (Either RefTypeClaims (Map TpId RefTypeClaims))
-- validatePath vs p mTpids = do
--     def <- first pure $ first GenericErr $ defForPath p vs
--     t <- maybe (Left [ProgrammingErr "Tainted but missing"]) Right $ Tree.treeLookup p $ vsTree vs
--     validateRoseTreeNode def t mTpids
validatePath = undefined


validatePostValues :: PostDefinition -> [[SomeWireValue]] -> ErrsM s [Text] ()
validatePostValues pd =
    void . abortsEither . sequence . join
    . fmtStrictZipError "post def arg type" "list of wire values"
    . strictZipWith validateValues (toList $ postDefArgs pd)

type PathCreates = Map Placeholder (Maybe Attributee, CreateOp)

validateCreates :: Creates -> VsM (Mos Path Placeholder, Creates)
validateCreates creates =
    fmap output $ sequence $ Map.mapWithKey perPath creates
  where
    perPath :: Path -> PathCreates -> VsM (PathCreates)
    perPath p m = do
      pd <- pathError p $ pathPostDef p
      validArgs <- fmap (Map.mapMaybe id) $ sequence $
        pathErrors p . validateArgs pd <$> m
      children <- pathError p $ Set.fromList <$> lookupChildren p
      pathErrors p $ Map.fromList <$> sortOutAfterDeps children validArgs

    validateArgs pd x@(_, OpCreate args _) = soften $
      validatePostValues pd args >> return x

    output :: Map Path PathCreates -> (Mos Path Placeholder, Creates)
    output m = (Mos.fromMap $ Map.keysSet <$> m, m)

-- Our semantics make list handling _horrendous_! Going back to instructions
-- would be better, as then we wouldn't need to do dependency resolution.
sortOutAfterDeps
  :: Set Seg -> PathCreates
  -> ErrsM s [Text] [(Placeholder, (Maybe Attributee, CreateOp))]
sortOutAfterDeps segs crops = do
    (validRoots, dependants) <- filterDuplicateTargets crops >>= partitionRoots
    (Map.toList validRoots <>) <$> go validRoots dependants
  where
    filterDuplicateTargets :: PathCreates -> ErrsM s [Text] PathCreates
    filterDuplicateTargets crops =
      let
        duplicates = fold $ Map.filter ((> 1) . Set.size) $ unMos $
          Mos.invertMap $ ocAfter . snd <$> crops
      in do
        unless (null duplicates) $ reports
          [ Text.pack $
            "Multiple create operations referenced the same position target: "
            ++ show duplicates
          ]
        return $ crops `Map.withoutKeys` duplicates

    partitionOnAfter
        :: (Maybe EPS -> Bool) -> PathCreates -> (PathCreates, PathCreates)
    partitionOnAfter f = Map.partition (f . ocAfter . snd)

    partitionRoots :: PathCreates -> ErrsM s [Text] (PathCreates, PathCreates)
    partitionRoots crops =
      let
        (roots, dependants) = partitionOnAfter
          (maybe True (either (`Map.notMember` crops) (const True))) crops
        (validRoots, invalidRoots) = partitionOnAfter
          (maybe True (either (const False) (`Set.member` segs))) roots
      in do
        unless (null invalidRoots) $ reports
          [ Text.pack $
            "Invalid reference targets: " ++ show (Map.keysSet invalidRoots)
          ]
        return (validRoots, dependants)

    go
      :: PathCreates -> PathCreates
      -> ErrsM s [Text] [(Placeholder, (Maybe Attributee, CreateOp))]
    go valid remaining
      | null remaining = return []
      | otherwise =
        let
          (good, remaining') = partitionOnAfter
            (maybe True (either (flip Map.member valid) (const True))) remaining
        in do
          unless (null good) $ reports
            -- We got stuck with only bad placeholders remaining
            [ Text.pack $
              "Invalid reference targets: " ++ show (Map.keysSet remaining')
            ]
          (Map.toList good <>) <$> go (valid <> good) remaining'


-- validateAndFilterCreates
--   :: Valuespace -> Creates -> (Map DataErrorIndex [ValidationErr], Creates)
-- validateAndFilterCreates vs creates =
--   let errMap = validateCreates vs creates in
--     ( Map.mapKeysMonotonic PathError $ fmap (uncurry CreateError) <$> errMap
--     , filterSubMap (Set.fromList . fmap fst <$> errMap) creates)

-- FIXME: handling all these nested maps turns out to be pain!
-- partitionEitherNestedMaps
--   :: Map k1 (Map k2 (Either a b))
--   -> (Map k1 (Map k2 a), Map k1 (Map k2 b))
-- partitionEitherNestedMaps mm =
--   let partitioned = partitionEithers <$> mm in
--     (fst <$> partitioned, snd <$> partitioned)

-- validateCreateAndCopAfters
--   :: Valuespace -> Creates -> ContOps (Either Placeholder Seg)
--   -> Map DataErrorIndex [ValidationErr]
-- validateCreateAndCopAfters vs creates cops =
--     Map.unionsWith (<>) [copRefAbsPhs, copRefAbsSegs, crRefAbsPhs, crRefAbsSegs]
--   where
--     soAfter (SoAfter mi) = mi
--     soAfter SoAbsent = Nothing

--     -- All the existing names that container operations have referenced (we need
--     -- to separate them out into Placeholders and Segs because we need to check
--     -- each in its resepective pool of defined names; we need to keep hold of
--     -- the paths from which they came for lookups and error messages):
--     copPhAfters :: Map Path (Map Seg Placeholder)
--     copSegAfters :: Map Path (Map Seg Seg)
--     (copPhAfters, copSegAfters) = partitionEitherNestedMaps copAfters
--     copAfters = fmap (Map.mapMaybe soAfter . fmap snd) cops

--     -- All the existing names that creates have referenced:
--     crPhAfters :: Map Path (Map Placeholder Placeholder)
--     crSegAfters :: Map Path (Map Placeholder Seg)
--     (crPhAfters, crSegAfters) = partitionEitherNestedMaps createAfters
--     createAfters = fmap (Map.mapMaybe ocAfter . fmap snd) creates

--     getCreatedPhs :: Path -> Set Placeholder
--     getCreatedPhs p = maybe mempty Map.keysSet $ Map.lookup p creates

--     removed :: Path -> Set Seg
--     removed p = case Map.lookup p cops of
--       Nothing -> mempty
--       Just pCops -> Map.keysSet $ Map.filter (isSoAbsent . snd) pCops

--     getExistingChildSegs :: Path -> Set Seg
--     getExistingChildSegs p = case Tree.treeLookupNode p $ vsTree vs of
--       Just (RtnChildren al) -> Set.difference (alKeysSet al) (removed p)
--       _ -> mempty

--     -- "Refs" == names referenced by an "after" somewhere
--     validatePathNameRefs
--       :: Ord i
--       => (k -> i -> ValidationErr) -> Set i -> Map k i -> [ValidationErr]
--     validatePathNameRefs mkErr definedNames afters = Map.elems $
--         Map.mapWithKey mkErr $ Map.filter (not . (`Set.member` definedNames)) afters

--     validateNameRefs
--       :: Ord i
--       => (k -> i -> ValidationErr) -> (Path -> Set i) -> Map Path (Map k i)
--       -> Map DataErrorIndex [ValidationErr]
--     validateNameRefs mkErr getDefinedNames = Map.mapKeysMonotonic PathError
--       . Map.mapWithKey (\p m -> validatePathNameRefs mkErr (getDefinedNames p) m)

--     copRefAbsPhs = validateNameRefs
--         (\seg afterPh -> MoveReferencedAbsentName seg $ Left afterPh)
--         getCreatedPhs copPhAfters
--     copRefAbsSegs = validateNameRefs
--         (\seg afterSeg -> MoveReferencedAbsentName seg $ Right afterSeg)
--         getExistingChildSegs copSegAfters
--     crRefAbsPhs = validateNameRefs
--         (\ph afterPh -> CreateReferencedAbsentName ph $ Left afterPh)
--         getCreatedPhs crPhAfters
--     crRefAbsSegs = validateNameRefs
--         (\ph afterSeg -> CreateReferencedAbsentName ph $ Right afterSeg)
--         getExistingChildSegs crSegAfters

-- filterSubMap
--   :: (Ord k1, Ord k2)
--   => Map k1 (Set k2) -> Map k1 (Map k2 a) -> Map k1 (Map k2 a)
-- filterSubMap = merge
--   dropMissing
--   preserveMissing
--   (zipWithMatched $ const $ flip Map.withoutKeys)

-- FIXME: this filtering is fiddly
-- filterByAfterErrs
--   :: Map DataErrorIndex [ValidationErr]
--   -> Creates -> ContOps (Either Placeholder Seg)
--   -> (Creates, ContOps (Either Placeholder Seg))
-- filterByAfterErrs errMap creates cops =
--     ( filterSubMap badPhs creates
--     , filterSubMap badSegs cops)
--   where
--     badKeys = Map.fromList $
--       bimap errPath (partitionEithers . fmap key) <$> Map.toList errMap
--     badPhs = Set.fromList . fst <$> badKeys
--     badSegs = Set.fromList . snd <$> badKeys
--     -- !!! FIXME: Deliberate partials whilst nothing better to hand
--     -- (I'm tired and have lost the will to carry on with this!)
--     errPath = \case PathError p -> p
--     key = \case
--       CreateReferencedAbsentName ph _ -> Left ph
--       MoveReferencedAbsentName seg _ -> Right seg

-- dropPlaceholder
--   :: SequenceOp (Either Placeholder Seg) -> Maybe (SequenceOp Seg)
-- dropPlaceholder = traverse (either (const Nothing) (Just))

-- removedPaths :: ContOps a -> Set Path
-- removedPaths cops = Set.fromList $ mconcat $ Map.elems $
--   Map.mapWithKey childPaths $
--   Map.keys . Map.filter isSoAbsent . fmap snd <$> cops

-- filterDdByDataErrIdx :: [DataErrorIndex] -> DataDigest -> DataDigest
-- filterDdByDataErrIdx errIdxs =
--       alFmapWithKey removeBadTps
--     . alFilterKey (not . (`Set.member` constErrs))
--   where
--     (constErrs, tpErrs) = bimap Map.keysSet (Map.mapMaybe id) $
--       Map.partition (== Nothing) errIdxMap
--     errIdxMap :: Map Path (Maybe (Set Word32))
--     errIdxMap = fmap flipSet $ unMos $ Mos.fromList $ mapMaybe procErrIdx
--       $ errIdxs
--     flipSet :: Eq a => Set (Maybe a) -> Maybe (Set a)
--     flipSet = fmap Set.fromAscList . sequence . Set.toAscList
--     procErrIdx :: DataErrorIndex -> Maybe (Path, Maybe Word32)
--     procErrIdx = \case
--       GlobalError -> Nothing
--       PathError p -> Just (p, Nothing)
--       TimePointError p tpid -> Just (p, Just tpid)

--     removeBadTps path change =
--       case Map.lookup path tpErrs of
--         Nothing -> change
--         Just badTpIds -> case change of
--          ConstChange _ _ -> error "internal error: tp errors for const change"
--          TimeChange m -> TimeChange $ Map.withoutKeys m badTpIds

data ProtoFrpDigest = ProtoFrpDigest
  { frpdData :: DataDigest
  , frpdCreates :: Creates
  , frpdContOps :: ContOps (Either Placeholder Seg)
  } deriving (Show, Eq)

-- processTrcUpdateDigest
--   :: Valuespace -> TrcUpdateDigest
--   -> (Map DataErrorIndex [ValidationErr], ProtoFrpDigest)
-- processTrcUpdateDigest vs trcud =
--   let
--     (createErrs, createsWithValidArgs) = validateAndFilterCreates vs $
--       trcudCreates trcud
--     copErrs = validateCreateAndCopAfters vs createsWithValidArgs $
--       trcudContOps trcud
--     (validCreates, validCops) =
--       filterByAfterErrs copErrs createsWithValidArgs $ trcudContOps trcud
--     validSegCops = fmap (Map.mapMaybe $ traverse dropPlaceholder) validCops
--     -- adding to the front of the path will not break uniqueness
--     validPaths = Set.difference
--       (maybe mempty (Set.fromList . treePaths Root) $
--         Tree.treeLookup Root $ vsTree vs) (removedPaths validCops)
--     (pathValidDd, nonExistantSets) = alPartitionWithKey
--       (\p _ -> Set.member p validPaths) $ trcudData trcud
--     (updateErrs, tree') = Tree.updateTreeWithDigest validSegCops pathValidDd $
--       vsTree vs
--     touched = opsTouched validSegCops pathValidDd
--     vs' = vs {vsTree = tree'}
--     touchedEditabilities = Map.mapWithKey (\k _ -> getEditable k vs') touched
--     roErrs = const [EditableErr "Touched read only"]
--       <$> Map.filter (== Just ReadOnly) touchedEditabilities
--     (validationErrs, refClaims) = Map.mapEitherWithKey (validatePath vs') touched
--     refErrs = either id (const mempty) $ checkRefClaims (vsTyAssns vs') refClaims

--     dataErrs = mappend refErrs $ Map.unionsWith (<>) $ fmap (Map.mapKeys PathError)
--       [ fmap (GenericErr . Text.unpack) <$> updateErrs
--       , validationErrs, roErrs
--       , Map.fromSet (const $ [TouchedNonExistantPath]) $
--           alKeysSet nonExistantSets
--       ]
--     errMap = Map.unionsWith (<>) [createErrs, copErrs, dataErrs]
--     pfrpd = ProtoFrpDigest
--       (filterDdByDataErrIdx (Map.keys dataErrs) pathValidDd)
--       validCreates
--       validCops
--   in (errMap, pfrpd)
processTrcud
  :: TrcUpdateDigest -> Valuespace
  -> (Mol DataErrorIndex Text, FrpDigest)
processTrcud trcud vs =
  let (e, frpd, _vs') = runErrsMSoft (processTrcud_ trcud) vs in
    (e, maybe (frpdEmpty $ trcudNamespace trcud) id frpd)

processTrcud_ :: TrcUpdateDigest -> VsM FrpDigest
processTrcud_ (TrcUpdateDigest ns dat crs cops) = do
  dat' <- guardClientUpdates dat
  (newPhsMos, crs') <- validateCreates crs
  cops' <- guardClientCops newPhsMos cops
  return $ FrpDigest ns dat' crs' cops'

type EPS = Either Placeholder Seg

guardClientCops
  :: Mos Path Placeholder -> ContOps EPS -> VsM (ContOps EPS)
guardClientCops newPhsMos =
    fmap (Map.mapMaybe id) . sequence . Map.mapWithKey perPath
  where
    perPath
      :: Path -> Map Seg (Maybe Attributee, SequenceOp EPS)
      -> VsM (Maybe (Map Seg (Maybe Attributee, SequenceOp EPS)))
    perPath p m =
      let
        seqOpMap :: Map Seg (SequenceOp EPS)
        seqOpMap = snd <$> m
        newPhs = Mos.lookup p newPhsMos
      in soften $ pathError p $ do
        SomeDefinition def <- pathDef p
        case def of
          ArrayDef {} -> do
            children <- Set.fromList <$> lookupChildren p
            let unexpectedChildren =
                  Map.keysSet seqOpMap `Set.difference` children
            unless (null unexpectedChildren) $ aborts $ Text.pack $
              printf "Unexpected children %s in array rearrangment" $
              show unexpectedChildren
            let unexpectedAfters = Map.elems $
                  Map.filter (not . validAfter newPhs children) seqOpMap
            unless (null unexpectedAfters) $ aborts $ Text.pack $
              printf "Unexpected 'afters' %s in array rearrangement" $
              show unexpectedAfters
            return m
          _ -> aborts "Array-rearrangement operation on non-array"
    validAfter
      :: Set Placeholder -> Set Seg
      -> SequenceOp (Either Placeholder Seg) -> Bool
    validAfter phs ss = \case
      SoAfter Nothing -> True
      SoAfter (Just i) -> either (flip Set.member phs) (flip Set.member ss) i
      SoAbsent -> True



guardClientUpdates
  :: AssocList Path DataChange -> VsM (AssocList Path DataChange)
guardClientUpdates =
  fmap (alMapMaybe id) . sequence . alFmapWithKey guardClientUpdate

guardClientUpdate :: Path -> DataChange -> VsM (Maybe DataChange)
guardClientUpdate p dc = soften $ do
  ed <- pathError p $ pathEditable p
  case ed of
    Editable -> updatePathData p dc
    ReadOnly -> pathError p $ aborts "Data change at read-only path"
  return dc

-- data ValidationErr
--   = GenericErr String
--   | ProgrammingErr String
--   | BadNodeType {vebntExpected :: RoseTreeNodeType, vebntActual :: RoseTreeNodeType}
--   | MissingChild Seg
--   | ExtraChild Seg
--   | TouchedNonExistantPath
--   | RefTargetNotFound Path
--   | RefTargetTypeErr
--     { veRttePath :: Path
--     , veRtteExpectedType :: DefName
--     , veRtteTargetType :: DefName}
--   | EditableErr String
--   | CreateReferencedAbsentName Placeholder (Either Placeholder Seg)
--   | MoveReferencedAbsentName Seg (Either Placeholder Seg)
--   -- FIXME: might want to make more specific creation errors. Currently can
--   -- arise from PostDef lookup failures:
--   | CreateError Placeholder String
--   | BadCreateArgs {vePh :: Placeholder, veArgName :: Seg}
--   deriving (Show)

-- defNodeType :: Definition mt -> RoseTreeNodeType
-- defNodeType def = case def of
--     StructDef {} -> RtntContainer
--     ArrayDef {} -> RtntContainer
--     TupleDef { tupDefILimit = ilimit } -> case ilimit of
--         ILUninterpolated -> RtntConstData
--         _ -> RtntDataSeries

-- validateRoseTreeNode
--   :: Definition -> RoseTree [WireValue] -> Maybe (Set TpId)
--   -> Either [ValidationErr] (Either RefTypeClaims (Map TpId RefTypeClaims))
-- validateRoseTreeNode def t invalidatedTps = case t of
--     RtEmpty -> tyErr
--     RtConstData _ wvs -> case def of
--       TupleDef (TupleDefinition _ alTreeTypes _) ->
--         first pure $ first GenericErr $
--         Left <$> validateWireValues (alValues alTreeTypes) wvs
--       _ -> tyErr
--     RtDataSeries m -> case def of
--       TupleDef (TupleDefinition _ alTreeTypes _) ->
--         let toValidate = case invalidatedTps of
--               Nothing -> Dkmap.valueMap m
--               Just tpids -> Map.restrictKeys (Dkmap.valueMap m) tpids
--         in first pure $ first GenericErr $
--           fmap Right $
--           mapM (validateWireValues (alValues alTreeTypes) . snd . snd) toValidate
--       _ -> tyErr
--     RtContainer alCont -> case def of
--       TupleDef _ -> tyErr
--       StructDef (StructDefinition _ alDef) -> if defSegs == rtnSegs
--           then return $ Left mempty
--           else Left $ fmap MissingChild missingSegs ++ fmap ExtraChild extraSegs
--         where
--           defSegs = alKeysSet alDef
--           rtnSegs = alKeysSet alCont
--           missingSegs = Set.toList $ Set.difference defSegs rtnSegs
--           extraSegs = Set.toList $ Set.difference rtnSegs defSegs
--       ArrayDef _ -> return $ Left mempty
--   where
--     tyErr = Left [BadNodeType (defNodeType def) (Tree.rtType t)]
validateRoseTreeNode = undefined

-- -- FIXME: this would be better if it would return more semantic errors
-- validateWireValues
--   :: MonadFail m => [SomeTreeType] -> [WireValue] -> m RefTypeClaims
-- validateWireValues tts wvs =
--     (fmtStrictZipError "types" "values" $ strictZipWith vr tts wvs)
--     >>= sequence >>= return . Mos.fromList . mconcat
--   where
--     vr (SomeTreeType tt) wv@(WireValue wt _) =
--       extractTypeAssertions tt <$> validate tt wv
validateWireValues = undefined

-- validateExistingXrefs
--   :: Xrefs -> Map Path DefName
--   -> Map DataErrorIndex [ValidationErr]
-- validateExistingXrefs xrs newTas =
--   let
--     retypedPaths = Map.keysSet newTas
--     invalidated = Map.restrictKeys xrs retypedPaths
--     errText referee = [GenericErr $
--       "Ref target changed type: " ++
--       Text.unpack (Path.toText Path.unSeg referee)]
--     refererErrors referee acc referer mTpids = case mTpids of
--       Nothing -> Map.insert (PathError referer) (errText referee) acc
--       Just tpids -> Set.foldl
--         (\acc' tpid ->
--            Map.insert (TimePointError referer tpid) (errText referee) acc')
--         acc tpids
--     refereeErrs acc referee refererMap =
--       Map.foldlWithKey (refererErrors referee) acc refererMap
--   in
--     Map.foldlWithKey refereeErrs mempty invalidated

{-# LANGUAGE
    FlexibleContexts
  , LambdaCase
#-}

module Clapi.Valuespace
  ( Valuespace, vsTree, vsTyDefs, vsPostDefs
  , baseValuespace
  , vsLookupPostDef, vsLookupDef, valuespaceGet, getLiberty
  , apiNs, apiTypeName, dnSeg
  , processToRelayProviderDigest, processTrcUpdateDigest
  , validateVs, unsafeValidateVs
  , vsRelinquish, ValidationErr(..)
  ) where

import Prelude hiding (fail)
import Control.Monad (unless, liftM2)
import Control.Monad.Fail (MonadFail(..))
import Data.Bifunctor (first, bimap)
import Data.Either (lefts, partitionEithers)
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Strict.Merge
  (merge, preserveMissing, dropMissing, zipWithMatched, zipWithMaybeMatched)
import Data.Maybe (fromJust, mapMaybe)
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word

import qualified Data.Map.Mol as Mol
import Data.Map.Mos (Mos)
import qualified Data.Map.Mos as Mos

import Data.Maybe.Clapi (note)

import Clapi.TH
import Clapi.Util (strictZipWith, fmtStrictZipError, mapPartitionEither)
import Clapi.Tree
  ( RoseTree(..), RoseTreeNode(..), treeInsert, treeChildren, TpId
  , RoseTreeNodeType(..), treePaths)
import qualified Clapi.Tree as Tree
import Clapi.Types (WireValue(..))
import Clapi.Types.Base (InterpolationLimit(ILUninterpolated))
import Clapi.Types.AssocList
  ( alKeysSet, alValues, alSingleton, alEmpty
  , unsafeMkAssocList, alMapKeys, alFmapWithKey, alToMap, alPartitionWithKey
  , alFilterKey)
import Clapi.Types.Definitions
  ( Definition(..), Liberty(..), TupleDefinition(..)
  , StructDefinition(..), ArrayDefinition(..), PostDefinition(..), defDispatch
  , childLibertyFor, childTypeFor)
import Clapi.Types.Digests
  ( DefOp(..), isUndef, ContOps, DataChange(..), isRemove, DataDigest
  , TrpDigest(..), trpdRemovedPaths, TrcUpdateDigest(..), CreateOp(..), Creates
  , FrpDigest(..))
import Clapi.Types.Messages (DataErrorIndex(..))
import Clapi.Types.Path
  ( Seg, Path, pattern (:/), pattern Root, pattern (:</), TypeName(..)
  , qualify, unqualify, tTnNamespace, Namespace(..), Placeholder, childPaths)
import qualified Clapi.Types.Path as Path
import Clapi.Types.SequenceOps (SequenceOp(..), isSoAbsent)
import Clapi.Types.Tree (TreeType(..), unbounded)
import Clapi.Validator (validate, extractTypeAssertions)
import qualified Clapi.Types.Dkmap as Dkmap

-- FIXME: this should probably be `Map (Tagged def TypeName) def`
type DefMap def = Map Namespace (Map (Tagged def Seg) def)
type TypeAssignmentMap = Mos.Dependencies Path (Tagged Definition TypeName)
type Referer = Path
type Referee = Path
type Xrefs = Map Referee (Map Referer (Maybe (Set TpId)))

data Valuespace = Valuespace
  { vsTree :: RoseTree [WireValue]
  , vsPostDefs :: DefMap PostDefinition
  , vsTyDefs :: DefMap Definition
  , vsTyAssns :: TypeAssignmentMap
  , vsXrefs :: Xrefs
  } deriving (Eq, Show)

removeTamSubtree :: TypeAssignmentMap -> Path -> TypeAssignmentMap
removeTamSubtree tam p = Mos.filterDependencies (not . flip Path.isChildOf p) tam

removeXrefs :: Referer -> Xrefs -> Xrefs
removeXrefs referer = fmap (Map.delete referer)

filterXrefs :: (Referer -> Bool) -> Xrefs -> Xrefs
filterXrefs f = fmap (Map.filterWithKey $ \k _ -> f k)

removeXrefsTps :: Referer -> Set TpId -> Xrefs -> Xrefs
removeXrefsTps referer tpids = fmap (Map.update updateTpMap referer)
  where
    updateTpMap Nothing = Just Nothing
    updateTpMap (Just tpSet) = let tpSet' = Set.difference tpSet tpids in
      if null tpSet' then Nothing else Just $ Just tpSet'

apiNs :: Namespace
apiNs = Namespace [segq|api|]

apiTypeName :: Tagged Definition TypeName
apiTypeName = Tagged $ TypeName apiNs $ unNamespace apiNs

apiDef :: StructDefinition
apiDef = StructDefinition "Information about CLAPI itself" $
  alSingleton [segq|version|] (Tagged $ TypeName apiNs [segq|version|], Cannot)

versionDef :: TupleDefinition
versionDef = TupleDefinition "The version of CLAPI" (unsafeMkAssocList
  [ ([segq|major|], TtWord32 unbounded)
  , ([segq|minor|], TtWord32 unbounded)
  , ([segq|revision|], TtInt32 unbounded)
  ]) ILUninterpolated

dnSeg :: Seg
dnSeg = [segq|display_name|]

displayNameDef :: TupleDefinition
displayNameDef = TupleDefinition
  "A human-readable name for a struct or array element"
  (alSingleton [segq|name|] $ TtString "") ILUninterpolated

-- | Fully revalidates the given Valuespace and throws an error if there are any
--   validation issues.
unsafeValidateVs :: Valuespace -> Valuespace
unsafeValidateVs vs = either (error . show) snd $ validateVs allTainted vs
  where
    allTainted = Map.fromList $ fmap (,Nothing) $ Tree.treePaths Root $
      vsTree vs

baseValuespace :: Valuespace
baseValuespace = unsafeValidateVs $
    Valuespace baseTree mempty baseDefs mempty mempty
  where
    vseg = [segq|version|]
    version = RtConstData Nothing
      [WireValue @Word32 0, WireValue @Word32 1, WireValue @Int32 (-1022)]
    baseTree =
      treeInsert Nothing (Root :/ unNamespace apiNs :/ vseg) version
      Tree.RtEmpty
    baseDefs = Map.singleton apiNs $ Map.fromList
      [ (Tagged $ unNamespace apiNs, StructDef apiDef)
      , (Tagged vseg, TupleDef versionDef)
      , (Tagged dnSeg, TupleDef displayNameDef)
      ]

lookupPostDef
  :: MonadFail m
  => Tagged PostDefinition TypeName -> DefMap PostDefinition -> m PostDefinition
lookupPostDef tn defs = let (ns, s) = unqualify tn in note "Missing post def" $
    Map.lookup ns defs >>= Map.lookup s

vsLookupPostDef
  :: MonadFail m
  => Tagged PostDefinition TypeName -> Valuespace -> m PostDefinition
vsLookupPostDef tn vs = lookupPostDef tn $ vsPostDefs vs

postDefForPath :: MonadFail m => Path -> Valuespace -> m PostDefinition
postDefForPath p vs = defForPath p vs >>=
  (\case
    ArrayDef ad -> maybe (fail "array does not define post type") return $
        arrPostType ad
    _ -> fail "Definition at path not for array") >>=
  flip vsLookupPostDef vs

lookupDef
  :: MonadFail m
  => Tagged Definition TypeName -> DefMap Definition -> m Definition
lookupDef tn defs = note "Missing def" $
    (Map.lookup ns defs >>= Map.lookup s)
  where
    (ns, s) = unqualify tn

vsLookupDef
  :: MonadFail m => Tagged Definition TypeName -> Valuespace -> m Definition
vsLookupDef tn vs = lookupDef tn $ vsTyDefs vs

lookupTypeName
  :: MonadFail m => Path -> TypeAssignmentMap -> m (Tagged Definition TypeName)
lookupTypeName p = note "Type name not found" . Mos.getDependency p

defForPath :: MonadFail m => Path -> Valuespace -> m Definition
defForPath p vs =
  lookupTypeName p (vsTyAssns vs) >>= flip lookupDef (vsTyDefs vs)

getLiberty :: MonadFail m => Path -> Valuespace -> m Liberty
getLiberty path vs = case path of
  Root :/ _ -> return Cannot
  p :/ s -> defForPath p vs >>= defDispatch (flip childLibertyFor s)
  _ -> return Cannot

valuespaceGet
  :: MonadFail m => Path -> Valuespace
  -> m ( Definition
       , Tagged Definition TypeName
       , Liberty
       , RoseTreeNode [WireValue])
valuespaceGet p vs@(Valuespace tree _ defs tas _) = do
    rtn <- note "Path not found" $ Tree.treeLookupNode p tree
    tn <- lookupTypeName p tas
    def <- lookupDef tn defs
    lib <- getLiberty p vs
    return (def, tn, lib, rtn)

type RefTypeClaims = Mos (Tagged Definition TypeName) Referee
type TypeClaimsByPath =
  Map Referer (Either RefTypeClaims (Map TpId RefTypeClaims))

partitionXrefs :: Xrefs -> TypeClaimsByPath -> (Xrefs, Xrefs)
partitionXrefs oldXrefs claims = (preExistingXrefs, newXrefs)
  where
    mosValues :: (Ord k, Ord v) => Mos k v -> Set v
    mosValues = foldl Set.union mempty . Map.elems
    newXrefs = Map.foldlWithKey asXref mempty claims
    asXref acc referer claimEither = case claimEither of
      Left rtcs -> foldl (addReferer referer) acc $ mosValues rtcs
      Right rtcm -> Map.foldlWithKey (addRefererWithTpid referer) acc rtcm
    addReferer referer acc referee = Map.alter
      (Just . Map.insert referer Nothing . maybe mempty id) referee acc
    addRefererWithTpid referer acc tpid rtcs =
      foldl (addRefererWithTpid' referer tpid) acc $ mosValues rtcs
    addRefererWithTpid' referer tpid acc referee = Map.alter
      (Just . Map.alter
        (Just . Just . maybe (Set.singleton tpid) (Set.insert tpid) .
          maybe Nothing id)
        referer . maybe mempty id)
      referee
      acc
    preExistingXrefs = Map.foldlWithKey removeOldXrefs oldXrefs claims
    removeOldXrefs acc referrer claimEither = case claimEither of
      Left _ -> removeXrefs referrer acc
      Right rtcm -> removeXrefsTps referrer (Map.keysSet rtcm) acc

xrefUnion :: Xrefs -> Xrefs -> Xrefs
xrefUnion = Map.unionWith $ Map.unionWith $ liftM2 Set.union

checkRefClaims
  :: TypeAssignmentMap
  -> Map Path (Either RefTypeClaims (Map TpId RefTypeClaims))
  -> Either (Map DataErrorIndex [ValidationErr]) ()
checkRefClaims tyAssns = smashErrMap . Map.mapWithKey checkRefsAtPath
  where
    errIf m = unless (null m) $ Left m
    smashErrMap = errIf . Mol.unions . lefts . Map.elems
    checkRefsAtPath
      :: Path
      -> Either RefTypeClaims (Map TpId RefTypeClaims)
      -> Either (Map DataErrorIndex [ValidationErr]) ()
    checkRefsAtPath path refClaims =
      let
        doCheck eidx = first (Map.singleton eidx . pure @[]) .
          mapM_ (uncurry checkRef) . Mos.toList
      in
        either
          (doCheck $ PathError path)
          (smashErrMap .
           Map.mapWithKey (\tpid -> doCheck (TimePointError path tpid)))
          refClaims
    checkRef :: Tagged Definition TypeName -> Path -> Either ValidationErr ()
    checkRef requiredTn refP = case lookupTypeName refP tyAssns of
        Nothing -> Left $ RefTargetNotFound refP
        Just actualTn -> if actualTn == requiredTn
            then Right ()
            else Left $ RefTargetTypeErr refP actualTn requiredTn

validateVs
  :: Map Path (Maybe (Set TpId)) -> Valuespace
  -> Either
       (Map DataErrorIndex [ValidationErr])
       (Map Path (Tagged Definition TypeName), Valuespace)
validateVs t v = do
    (newTypeAssns, refClaims, vs) <-
      -- FIXME: there is now no root type, so how do we change this?
      -- As the root type is dynamic we always treat it as if it has been
      -- redefined:
      inner mempty mempty (Map.insert Root Nothing t) v
    checkRefClaims (vsTyAssns vs) refClaims
    let (preExistingXrefs, newXrefs) = partitionXrefs (vsXrefs vs) refClaims
    let existingXrefErrs = validateExistingXrefs preExistingXrefs newTypeAssns
    unless (null existingXrefErrs) $ Left existingXrefErrs
    let vs' = vs {vsXrefs = xrefUnion preExistingXrefs newXrefs}
    return (newTypeAssns, vs')
  where
    errP p = first (Map.singleton (PathError p) . pure . GenericErr)
    tLook p = maybe (Left $ Map.singleton (PathError p) [ProgrammingErr "Missing RTN"]) Right .
      Tree.treeLookup p
    changed :: Eq a => a -> a -> Maybe a
    changed a1 a2 | a1 == a2 = Nothing
                  | otherwise = Just a2

    -- FIXME: this currently bails earlier than it could in light of validation
    -- errors. If we can't figure out the type of children in order to recurse,
    -- then we should probably stop, but if we just encouter bad data, we should
    -- probably just capture the error and continue.
    inner
      :: Map Path (Tagged Definition TypeName)
      -> TypeClaimsByPath
      -> Map Path (Maybe (Set TpId)) -> Valuespace
      -> Either (Map DataErrorIndex [ValidationErr])
           (Map Path (Tagged Definition TypeName), TypeClaimsByPath, Valuespace)
    inner newTas newRefClaims tainted vs =
      let tree = vsTree vs; oldTyAssns = vsTyAssns vs in
      case Map.toAscList tainted of
        [] -> return (newTas, newRefClaims, vs)
        ((path, invalidatedTps):_) ->
          case lookupTypeName path (vsTyAssns vs) of
            -- When we don't have the type (and haven't bailed out) we know the
            -- parent was implictly added by the rose tree and thus doesn't
            -- appear in the taints, but because it was changed it should be
            -- counted as such.
            Nothing -> case path of
              (parentPath :/ _) -> inner
                newTas newRefClaims (Map.insert parentPath Nothing tainted)
                vs
              _ -> Left $ Map.singleton GlobalError
                [GenericErr "Attempted to taint parent of root"]
            Just tn -> do
              def <- errP path $ vsLookupDef tn vs
              rtn <- tLook path tree
              case validateRoseTreeNode def rtn invalidatedTps of
                Left validationErrs -> if null emptyArrays
                    then Left $ Map.singleton (PathError path) validationErrs
                    else inner newTas' newRefClaims tainted' vs'
                  where
                    emptyArrays = mapMaybe mHandlable validationErrs
                    isEmptyContainer d = case d of
                        ArrayDef _ -> True
                        StructDef (StructDefinition _ defKids) -> defKids == alEmpty
                        _ -> False
                    mHandlable ve = case ve of
                        MissingChild name -> do
                          ctn <- defDispatch (childTypeFor name) def
                          cdef <- vsLookupDef ctn vs
                          if isEmptyContainer cdef
                            then Just (name, ctn)
                            else Nothing
                        _ -> Nothing
                    qEmptyArrays = first (path :/) <$> emptyArrays
                    newTas' = newTas <> Map.fromList qEmptyArrays
                    tainted' = Map.fromList (fmap (const Nothing) <$> qEmptyArrays) <> tainted
                    att = Nothing  -- FIXME: who is this attributed to?
                    insertEmpty childPath = treeInsert att childPath (RtContainer alEmpty)
                    vs' = vs {vsTree = foldl (\acc (cp, _) -> insertEmpty cp acc) tree qEmptyArrays}
                Right pathRefClaims -> inner
                      (newTas <> changedChildPaths)
                      (Map.insert path pathRefClaims newRefClaims)
                      (Map.delete path $
                         tainted <> fmap (const Nothing) changedChildPaths)
                      (vs {vsTyAssns = Mos.setDependencies changedChildPaths oldTyAssns})
                  where
                    oldChildTypes = Map.mapMaybe id $ alToMap $ alFmapWithKey
                      (\name _ -> Mos.getDependency (path :/ name) oldTyAssns) $
                      treeChildren rtn
                    newChildTypes = Map.mapMaybe id $ alToMap $ alFmapWithKey
                      (\name _ -> defDispatch (childTypeFor name) def) $
                      treeChildren rtn
                    changedChildTypes = merge
                      dropMissing preserveMissing
                      (zipWithMaybeMatched $ const changed)
                      oldChildTypes newChildTypes
                    changedChildPaths = Map.mapKeys (path :/) changedChildTypes

opsTouched :: ContOps Seg -> DataDigest -> Map Path (Maybe (Set TpId))
opsTouched cops dd = fmap (const Nothing) cops <> fmap classifyDc (alToMap dd)
  where
    classifyDc :: DataChange -> Maybe (Set TpId)
    classifyDc (ConstChange {}) = Nothing
    classifyDc (TimeChange m) = Just $ Map.keysSet m

updateNsDefs
  :: Map (Tagged def Seg) (DefOp def)
  -> Maybe (Map (Tagged def Seg) def)
  -> Maybe (Map (Tagged def Seg) def)
updateNsDefs defOps = Just . maybe newDefs (\existingDefs ->
    Map.union newDefs $ Map.withoutKeys existingDefs $ Map.keysSet unDefs)
  where
    (unDefs, defs) = Map.partition isUndef defOps
    newDefs = odDef <$> defs

processToRelayProviderDigest
  :: TrpDigest -> Valuespace
  -> Either
      (Map DataErrorIndex [Text])
      (Map Path (Tagged Definition TypeName), Valuespace)
processToRelayProviderDigest trpd vs =
  let
    ns = trpdNamespace trpd
    tas = foldl removeTamSubtree (vsTyAssns vs) $ trpdRemovedPaths trpd
    redefdPaths = mconcat $
      fmap (\s -> Mos.getDependants (qualify ns s) tas) $ Map.keys $
      trpdDefinitions trpd
    qData = fromJust $ alMapKeys (unNamespace ns :</) $ trpdData trpd
    qCops = Map.mapKeys (unNamespace ns :</) $ trpdContOps trpd
    updatedPaths = opsTouched qCops qData
    tpRemovals :: DataChange -> Set TpId
    tpRemovals (ConstChange {})= mempty
    tpRemovals (TimeChange m) = Map.keysSet $ Map.filter (isRemove . snd) m
    xrefs' = Map.foldlWithKey' (\x r ts -> removeXrefsTps r ts x) (vsXrefs vs) $
      fmap tpRemovals $ alToMap qData
    defs' = Map.alter (updateNsDefs $ trpdDefinitions trpd) ns (vsTyDefs vs)
    postDefs' = Map.alter (updateNsDefs $ trpdPostDefs trpd) ns (vsPostDefs vs)
    (updateErrs, tree') = Tree.updateTreeWithDigest qCops qData (vsTree vs)
  in do
    unless (null updateErrs) $ Left $ Map.mapKeys PathError updateErrs
    (updatedTypes, vs') <- first (fmap $ fmap $ Text.pack . show) $ validateVs
      (Map.fromSet (const Nothing) redefdPaths <> updatedPaths) $
      Valuespace tree' postDefs' defs' tas xrefs'
    return (updatedTypes, vs')

validatePath :: Valuespace -> Path -> Maybe (Set TpId) -> Either [ValidationErr] (Either RefTypeClaims (Map TpId RefTypeClaims))
validatePath vs p mTpids = do
    def <- first pure $ first GenericErr $ defForPath p vs
    t <- maybe (Left [ProgrammingErr "Tainted but missing"]) Right $ Tree.treeLookup p $ vsTree vs
    validateRoseTreeNode def t mTpids

-- | Returns an intermediary error structure
validateCreates :: Valuespace -> Creates -> Map Path [(Placeholder, String)]
validateCreates vs creates = fmap mconcat $
    Map.mapWithKey (\pth -> Map.elems . Map.mapWithKey (getArgValidator pth)) $
    fmap (ocArgs . snd) <$> creates
  where
    getArgValidator
      :: Path -> Placeholder -> [WireValue] -> [(Placeholder, String)]
    getArgValidator path ph =
      either (const . pure . (ph,)) (validatorFromPd ph) $
      postDefForPath path vs

    -- FIXME: finer-grained errors from value validation would be preferable
    validatorFromPd
      :: Placeholder -> PostDefinition -> [WireValue] -> [(Placeholder, String)]
    validatorFromPd ph pd = either pure mempty
      . first (ph,) . validateWireValues (alValues $ postDefArgs pd)

validateAndFilterCreates
  :: Valuespace -> Creates -> (Map DataErrorIndex [ValidationErr], Creates)
validateAndFilterCreates vs creates =
  let errMap = validateCreates vs creates in
    ( Map.mapKeysMonotonic PathError $ fmap (uncurry CreateError) <$> errMap
    , filterSubMap (Set.fromList . fmap fst <$> errMap) creates)

-- FIXME: handling all these nested maps turns out to be pain!
partitionEitherNestedMaps
  :: Map k1 (Map k2 (Either a b))
  -> (Map k1 (Map k2 a), Map k1 (Map k2 b))
partitionEitherNestedMaps mm =
  let partitioned = mapPartitionEither <$> mm in
    (fst <$> partitioned, snd <$> partitioned)

validateCreateAndCopAfters
  :: Valuespace -> Creates -> ContOps (Either Placeholder Seg)
  -> Map DataErrorIndex [ValidationErr]
validateCreateAndCopAfters vs creates cops =
    Map.unionsWith (<>) [copRefAbsPhs, copRefAbsSegs, crRefAbsPhs, crRefAbsSegs]
  where
    soAfter (SoAfter mi) = mi
    soAfter SoAbsent = Nothing

    -- All the existing names that container operations have referenced (we need
    -- to separate them out into Placeholders and Segs because we need to check
    -- each in its resepective pool of defined names; we need to keep hold of
    -- the paths from which they came for lookups and error messages):
    copPhAfters :: Map Path (Map Seg Placeholder)
    copSegAfters :: Map Path (Map Seg Seg)
    (copPhAfters, copSegAfters) = partitionEitherNestedMaps copAfters
    copAfters = fmap (Map.mapMaybe soAfter . fmap snd) cops

    -- All the existing names that creates have referenced:
    crPhAfters :: Map Path (Map Placeholder Placeholder)
    crSegAfters :: Map Path (Map Placeholder Seg)
    (crPhAfters, crSegAfters) = partitionEitherNestedMaps createAfters
    createAfters = fmap (Map.mapMaybe ocAfter . fmap snd) creates

    getCreatedPhs :: Path -> Set Placeholder
    getCreatedPhs p = maybe mempty Map.keysSet $ Map.lookup p creates

    removed :: Path -> Set Seg
    removed p = case Map.lookup p cops of
      Nothing -> mempty
      Just pCops -> Map.keysSet $ Map.filter (isSoAbsent . snd) pCops

    getExistingChildSegs :: Path -> Set Seg
    getExistingChildSegs p = case Tree.treeLookupNode p $ vsTree vs of
      Just (RtnChildren al) -> Set.difference (alKeysSet al) (removed p)
      _ -> mempty

    -- "Refs" == names referenced by an "after" somewhere
    validatePathNameRefs
      :: Ord i
      => (k -> i -> ValidationErr) -> Set i -> Map k i -> [ValidationErr]
    validatePathNameRefs mkErr definedNames afters = Map.elems $
        Map.mapWithKey mkErr $ Map.filter (not . (`Set.member` definedNames)) afters

    validateNameRefs
      :: Ord i
      => (k -> i -> ValidationErr) -> (Path -> Set i) -> Map Path (Map k i)
      -> Map DataErrorIndex [ValidationErr]
    validateNameRefs mkErr getDefinedNames = Map.mapKeysMonotonic PathError
      . Map.mapWithKey (\p m -> validatePathNameRefs mkErr (getDefinedNames p) m)

    copRefAbsPhs = validateNameRefs
        (\seg afterPh -> MoveReferencedAbsentName seg $ Left afterPh)
        getCreatedPhs copPhAfters
    copRefAbsSegs = validateNameRefs
        (\seg afterSeg -> MoveReferencedAbsentName seg $ Right afterSeg)
        getExistingChildSegs copSegAfters
    crRefAbsPhs = validateNameRefs
        (\ph afterPh -> CreateReferencedAbsentName ph $ Left afterPh)
        getCreatedPhs crPhAfters
    crRefAbsSegs = validateNameRefs
        (\ph afterSeg -> CreateReferencedAbsentName ph $ Right afterSeg)
        getExistingChildSegs crSegAfters

filterSubMap
  :: (Ord k1, Ord k2)
  => Map k1 (Set k2) -> Map k1 (Map k2 a) -> Map k1 (Map k2 a)
filterSubMap = merge
  dropMissing
  preserveMissing
  (zipWithMatched $ const $ flip Map.withoutKeys)

-- FIXME: this filtering is fiddly
filterByAfterErrs
  :: Map DataErrorIndex [ValidationErr]
  -> Creates -> ContOps (Either Placeholder Seg)
  -> (Creates, ContOps (Either Placeholder Seg))
filterByAfterErrs errMap creates cops =
    ( filterSubMap badPhs creates
    , filterSubMap badSegs cops)
  where
    badKeys = Map.fromList $
      bimap errPath (partitionEithers . fmap key) <$> Map.toList errMap
    badPhs = Set.fromList . fst <$> badKeys
    badSegs = Set.fromList . snd <$> badKeys
    -- !!! FIXME: Deliberate partials whilst nothing better to hand
    -- (I'm tired and have lost the will to carry on with this!)
    errPath = \case PathError p -> p
    key = \case
      CreateReferencedAbsentName ph _ -> Left ph
      MoveReferencedAbsentName seg _ -> Right seg

dropPlaceholder
  :: SequenceOp (Either Placeholder Seg) -> Maybe (SequenceOp Seg)
dropPlaceholder = traverse (either (const Nothing) (Just))

removedPaths :: ContOps a -> Set Path
removedPaths cops = Set.fromList $ mconcat $ Map.elems $
  Map.mapWithKey childPaths $
  Map.keys . Map.filter isSoAbsent . fmap snd <$> cops

filterDdByDataErrIdx :: [DataErrorIndex] -> DataDigest -> DataDigest
filterDdByDataErrIdx errIdxs =
      alFmapWithKey removeBadTps
    . alFilterKey (not . (`Set.member` constErrs))
  where
    (constErrs, tpErrs) = bimap Map.keysSet (Map.mapMaybe id) $
      Map.partition (== Nothing) errIdxMap
    errIdxMap :: Map Path (Maybe (Set Word32))
    errIdxMap = fmap flipSet $ Mos.fromList $ mapMaybe procErrIdx $ errIdxs
    flipSet :: Eq a => Set (Maybe a) -> Maybe (Set a)
    flipSet = fmap Set.fromAscList . sequence . Set.toAscList
    procErrIdx :: DataErrorIndex -> Maybe (Path, Maybe Word32)
    procErrIdx = \case
      GlobalError -> Nothing
      PathError p -> Just (p, Nothing)
      TimePointError p tpid -> Just (p, Just tpid)

    removeBadTps path change =
      case Map.lookup path tpErrs of
        Nothing -> change
        Just badTpIds -> case change of
         ConstChange _ _ -> error "internal error: tp errors for const change"
         TimeChange m -> TimeChange $ Map.withoutKeys m badTpIds

processTrcUpdateDigest
  :: Valuespace -> TrcUpdateDigest
  -> (Map DataErrorIndex [ValidationErr], FrpDigest)
processTrcUpdateDigest vs trcud =
  let
    ns = unNamespace $ trcudNamespace trcud
    (createErrs, createsWithValidArgs) = validateAndFilterCreates vs $
      trcudCreates trcud
    afterErrs = validateCreateAndCopAfters vs createsWithValidArgs $
      trcudContOps trcud
    (validCreates, validCops) =
      filterByAfterErrs afterErrs createsWithValidArgs $ trcudContOps trcud
    validSegCops = fmap (Map.mapMaybe $ traverse dropPlaceholder) validCops
    -- adding to the front of the path will not break uniqueness
    validPaths = Set.difference
      (maybe mempty (Set.fromList . treePaths Root) $
        Tree.treeLookup (Root :/ ns) $ vsTree vs) (removedPaths validCops)
    (pathValidDd, nonExistantSets) = alPartitionWithKey
      (\p _ -> Set.member p validPaths) $ trcudData trcud
    (updateErrs, tree') = Tree.updateTreeWithDigest validSegCops pathValidDd $
      vsTree vs
    touched = opsTouched validSegCops pathValidDd
    (tas', newPaths) = fillTyAssns
      (vsTyDefs vs) (vsTyAssns vs) (Map.keys touched)
    vs' = vs {vsTree = tree', vsTyAssns = tas'}
    touchedLiberties = Map.mapWithKey (\k _ -> getLiberty k vs') touched
    cannotErrs = const [LibertyErr "Touched a cannot"]
      <$> Map.filter (== Just Cannot) touchedLiberties
    -- FIXME: this will change with POST/create
    mustErrs = const [LibertyErr "Failed to provide a value for must"] <$>
      ( Map.filter (== Just Must)
      $ Map.fromSet (flip getLiberty vs') $ Set.fromList
      $ Tree.treeMissing tree')
    (validationErrs, refClaims) = Map.mapEitherWithKey (validatePath vs') touched
    refErrs = either id (const mempty) $ checkRefClaims (vsTyAssns vs') refClaims

    thing = Map.unionsWith (<>) $ fmap (Map.mapKeys PathError)
      [ fmap (GenericErr . Text.unpack) <$> updateErrs
      , validationErrs, cannotErrs, mustErrs
      ]
    errMap = Map.unionsWith (<>) [createErrs, afterErrs, thing]
    frpd = FrpDigest
      (trcudNamespace trcud)
      (filterDdByDataErrIdx (Map.keys thing) pathValidDd)
      validCreates
      validCops
  in (errMap, frpd)

fillTyAssns
  :: DefMap Definition -> TypeAssignmentMap -> [Path]
  -> (TypeAssignmentMap, Set Path)
fillTyAssns defs = inner mempty
  where
    inner freshlyAssigned tam paths = case paths of
        [] -> (tam, freshlyAssigned)
        (p : ps) -> case Mos.getDependency p tam of
            Just _ -> inner freshlyAssigned tam ps
            Nothing -> let newAssns = infer p $ fst tam in
                inner (freshlyAssigned <> Map.keysSet newAssns) (Mos.setDependencies newAssns tam) ps
    infer p tm = case Path.splitTail p of
        Nothing -> error "Attempted to infer root in fillTyAssns"
        Just (pp, cSeg) ->
          let
            (mptn, tm') = case Map.lookup pp tm of
                Just tn -> (Just tn, tm)
                Nothing -> let ptm = infer pp tm in
                    (Map.lookup pp ptm, tm <> ptm)
          in case mptn >>= flip lookupDef defs >>= defDispatch (childTypeFor cSeg) of
            Just tn -> Map.insert p tn tm'
            Nothing -> tm'

data ValidationErr
  = GenericErr String
  | ProgrammingErr String
  | BadNodeType {vebntExpected :: RoseTreeNodeType, vebntActual :: RoseTreeNodeType}
  | MissingChild Seg
  | ExtraChild Seg
  | RefTargetNotFound Path
  | RefTargetTypeErr
    { veRttePath :: Path
    , veRtteExpectedType :: Tagged Definition TypeName
    , veRtteTargetType :: Tagged Definition TypeName}
  | LibertyErr String
  | CreateReferencedAbsentName Placeholder (Either Placeholder Seg)
  | MoveReferencedAbsentName Seg (Either Placeholder Seg)
  -- FIXME: might want to make more specific creation errors. Currently can
  -- arise from PostDef lookup failures:
  | CreateError Placeholder String
  | BadCreateArgs {vePh :: Placeholder, veArgName :: Seg}
  deriving (Show)

defNodeType :: Definition -> RoseTreeNodeType
defNodeType def = case def of
    StructDef _ -> RtntContainer
    ArrayDef _ -> RtntContainer
    TupleDef (TupleDefinition _ _ interpLim) -> case interpLim of
        ILUninterpolated -> RtntConstData
        _ -> RtntDataSeries

validateRoseTreeNode
  :: Definition -> RoseTree [WireValue] -> Maybe (Set TpId)
  -> Either [ValidationErr] (Either RefTypeClaims (Map TpId RefTypeClaims))
validateRoseTreeNode def t invalidatedTps = case t of
    RtEmpty -> tyErr
    RtConstData _ wvs -> case def of
      TupleDef (TupleDefinition _ alTreeTypes _) ->
        first pure $ first GenericErr $
        Left <$> validateWireValues (alValues alTreeTypes) wvs
      _ -> tyErr
    RtDataSeries m -> case def of
      TupleDef (TupleDefinition _ alTreeTypes _) ->
        let toValidate = case invalidatedTps of
              Nothing -> Dkmap.valueMap m
              Just tpids -> Map.restrictKeys (Dkmap.valueMap m) tpids
        in first pure $ first GenericErr $
          fmap Right $
          mapM (validateWireValues (alValues alTreeTypes) . snd . snd) toValidate
      _ -> tyErr
    RtContainer alCont -> case def of
      TupleDef _ -> tyErr
      StructDef (StructDefinition _ alDef) -> if defSegs == rtnSegs
          then return $ Left mempty
          else Left $ fmap MissingChild missingSegs ++ fmap ExtraChild extraSegs
        where
          defSegs = alKeysSet alDef
          rtnSegs = alKeysSet alCont
          missingSegs = Set.toList $ Set.difference defSegs rtnSegs
          extraSegs = Set.toList $ Set.difference rtnSegs defSegs
      ArrayDef _ -> return $ Left mempty
  where
    tyErr = Left [BadNodeType (defNodeType def) (Tree.rtType t)]

-- FIXME: this would be better if it would return more semantic errors
validateWireValues
  :: MonadFail m => [TreeType] -> [WireValue] -> m RefTypeClaims
validateWireValues tts wvs =
    (fmtStrictZipError "types" "values" $ strictZipWith vr tts wvs)
    >>= sequence >>= return . Mos.fromList . mconcat
  where
    vr tt wv = validate tt wv >> extractTypeAssertions tt wv

-- FIXME: The VS you get back from this can be invalid WRT refs/inter NS types
vsRelinquish :: Namespace -> Valuespace -> Valuespace
vsRelinquish ns (Valuespace tree postDefs defs tas xrefs) =
  let
    nsp = Root :/ unNamespace ns
  in
    Valuespace
      (Tree.treeDelete nsp tree)
      (Map.delete ns postDefs)
      (Map.delete ns defs)
      (Mos.filterDeps
       (\p ttn -> not $ p `Path.isChildOf` nsp || ns == tTnNamespace ttn) tas)
      (filterXrefs (\p -> not (p `Path.isChildOf` nsp)) xrefs)

validateExistingXrefs
  :: Xrefs -> Map Path (Tagged Definition TypeName)
  -> Map DataErrorIndex [ValidationErr]
validateExistingXrefs xrs newTas =
  let
    retypedPaths = Map.keysSet newTas
    invalidated = Map.restrictKeys xrs retypedPaths
    errText referee = [GenericErr $
      "Ref target changed type: " ++
      Text.unpack (Path.toText Path.unSeg referee)]
    refererErrors referee acc referer mTpids = case mTpids of
      Nothing -> Map.insert (PathError referer) (errText referee) acc
      Just tpids -> Set.foldl
        (\acc' tpid ->
           Map.insert (TimePointError referer tpid) (errText referee) acc')
        acc tpids
    refereeErrs acc referee refererMap =
      Map.foldlWithKey (refererErrors referee) acc refererMap
  in
    Map.foldlWithKey refereeErrs mempty invalidated

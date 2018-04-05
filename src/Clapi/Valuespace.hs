{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Clapi.Valuespace
  ( Valuespace, vsTree, vsTyDefs
  , baseValuespace
  , vsLookupDef, valuespaceGet, getLiberty
  , apiNs, rootTypeName, apiTypeName
  , processToRelayProviderDigest, processToRelayClientDigest
  , validateVs, unsafeValidateVs
  , vsRelinquish
  ) where

import Prelude hiding (fail)
import Control.Applicative ((<|>))
import Control.Monad (unless, liftM2)
import Control.Monad.Fail (MonadFail(..))
import Data.Bifunctor (first)
import Data.Either (lefts)
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Strict.Merge
  (merge, preserveMissing, dropMissing, zipWithMaybeMatched)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word

import qualified Data.Map.Mol as Mol
import Data.Map.Mos (Mos)
import qualified Data.Map.Mos as Mos

import Data.Maybe.Clapi (note)

import Clapi.TH
import Clapi.Util (strictZipWith, fmtStrictZipError)
import Clapi.Tree (RoseTree(..), RoseTreeNode, treeInsert, treeChildren, TpId)
import qualified Clapi.Tree as Tree
import Clapi.Types (WireValue(..))
import Clapi.Types.Base (InterpolationLimit(ILUninterpolated))
import Clapi.Types.AssocList
  ( alKeysSet, alValues, alLookup, alFromMap, alSingleton
  , unsafeMkAssocList, alMapKeys, alFmapWithKey, alToMap)
import Clapi.Types.Definitions
  ( Definition(..), Liberty(..), TupleDefinition(..)
  , StructDefinition(..), ArrayDefinition(..), defDispatch, childLibertyFor
  , childTypeFor)
import Clapi.Types.Digests
  ( DefOp(..), isUndef, ContainerOps, DataChange(..), isRemove, DataDigest
  , TrpDigest(..), trpdRemovedPaths)
import Clapi.Types.Messages (ErrorIndex(..))
import Clapi.Types.Path
  (Seg, Path, pattern (:/), pattern Root, pattern (:</), TypeName(..))
import qualified Clapi.Types.Path as Path
import Clapi.Types.Tree (TreeType(..), ttWord32, ttInt32, unbounded)
import Clapi.Validator (validate, extractTypeAssertion)
import qualified Clapi.Types.Dkmap as Dkmap

type DefMap = Map Seg (Map Seg Definition)
type TypeAssignmentMap = Mos.Dependencies Path TypeName
type Referer = Path
type Referee = Path
type Xrefs = Map Referee (Map Referer (Maybe (Set TpId)))

data Valuespace = Valuespace
  { vsTree :: RoseTree [WireValue]
  , vsTyDefs :: DefMap
  , vsTyAssns :: TypeAssignmentMap
  , vsXrefs :: Xrefs
  } deriving (Eq, Show)

removeTamSubtree :: TypeAssignmentMap -> Path -> TypeAssignmentMap
removeTamSubtree tam p = Mos.filterDependencies (`Path.isChildOf` p) tam

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

apiNs :: Seg
apiNs = [segq|api|]

rootTypeName, apiTypeName :: TypeName
rootTypeName = TypeName apiNs [segq|root|]
apiTypeName = TypeName apiNs apiNs

apiDef :: StructDefinition
apiDef = StructDefinition "Information about CLAPI itself" $
  alSingleton [segq|version|] (TypeName apiNs [segq|version|], Cannot)

versionDef :: TupleDefinition
versionDef = TupleDefinition "The version of CLAPI" (unsafeMkAssocList
  [ ([segq|major|], ttWord32 unbounded)
  , ([segq|minor|], ttWord32 unbounded)
  , ([segq|revision|], ttInt32 unbounded)
  ]) ILUninterpolated

-- | Fully revalidates the given Valuespace and throws an error if there are any
--   validation issues.
unsafeValidateVs :: Valuespace -> Valuespace
unsafeValidateVs vs = either (error . show) snd $ validateVs allTainted vs
  where
    allTainted = Map.fromList $ fmap (,Nothing) $ Tree.treePaths Root $
      vsTree vs

baseValuespace :: Valuespace
baseValuespace = unsafeValidateVs $ Valuespace baseTree baseDefs baseTas mempty
  where
    vseg = [segq|version|]
    version = RtConstData Nothing
      [WireValue @Word32 0, WireValue @Word32 1, WireValue @Int32 (-1023)]
    baseTree =
      treeInsert Nothing (Root :/ apiNs :/ vseg) version Tree.RtEmpty
    baseDefs = Map.singleton apiNs $ Map.fromList
      [ (apiNs, StructDef apiDef)
      , (vseg, TupleDef versionDef)
      ]
    baseTas = Mos.dependenciesFromMap $ Map.fromList [(Root, rootTypeName)]

getTypeAssignment :: MonadFail m => DefMap -> Path -> m TypeName
getTypeAssignment defs thePath = lookupDef rootTypeName defs >>= go thePath
  where
    go path def = case path of
      seg :</ p -> do
        tn <- tnForChild def seg
        def' <- lookupDef tn defs
        case p of
          Root -> return tn
          _ -> go p def'
      _ -> return rootTypeName
    tnForChild def seg = case def of
      TupleDef _ -> fail "Tuples have no children"
      StructDef (StructDefinition _ tal) -> fst <$> alLookup seg tal
      ArrayDef (ArrayDefinition _ tn _) -> return tn

lookupDef :: MonadFail m => TypeName -> DefMap -> m Definition
lookupDef tn@(TypeName ns s) defs = note "Missing def" $
    (Map.lookup ns defs >>= Map.lookup s) <|>
    if tn == rootTypeName then Just rootDef else Nothing
  where
    -- NB: We generate the root def on the fly when people ask about it
    rootDef = StructDef $ StructDefinition "root def doc" $ alFromMap $
      Map.mapWithKey (\k _ -> (TypeName k k, Cannot)) defs

vsLookupDef :: MonadFail m => TypeName -> Valuespace -> m Definition
vsLookupDef tn vs = lookupDef tn $ vsTyDefs vs

lookupTypeName :: MonadFail m => Path -> TypeAssignmentMap -> m TypeName
lookupTypeName p = note "Type name not found" . Mos.getDependency p

defForPath :: MonadFail m => Path -> Valuespace -> m Definition
defForPath p (Valuespace _ defs tas _) =
  lookupTypeName p tas >>= flip lookupDef defs

getLiberty :: MonadFail m => Path -> Valuespace -> m Liberty
getLiberty path vs = case path of
  Root :/ _ -> return Cannot
  p :/ s -> defForPath p vs >>= defDispatch (flip childLibertyFor s)
  _ -> return Cannot

valuespaceGet
  :: MonadFail m => Path -> Valuespace
  -> m (Definition, TypeName, Liberty, RoseTreeNode [WireValue])
valuespaceGet p vs@(Valuespace tree defs tas _) = do
    rtn <- note "Path not found" $ Tree.treeLookupNode p tree
    tn <- lookupTypeName p tas
    def <- lookupDef tn defs
    lib <- getLiberty p vs
    return (def, tn, lib, rtn)

type RefTypeClaims = Mos TypeName Referee
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
  :: Valuespace -> Map Path (Either RefTypeClaims (Map TpId RefTypeClaims)) -> Either (Map (ErrorIndex TypeName) [Text]) ()
checkRefClaims vs refClaims = smashErrMap $ Map.mapWithKey (checkRefsAtPath (vsTree vs) (vsTyDefs vs)) refClaims
  where
    errIf m = unless (null m) $ Left m
    smashErrMap = errIf . Mol.unions . lefts . Map.elems
    checkRefsAtPath
      :: RoseTree [WireValue] -> DefMap -> Path
      -> Either RefTypeClaims (Map TpId RefTypeClaims)
      -> Either (Map (ErrorIndex TypeName) [Text]) ()
    checkRefsAtPath tree defs path refClaims =
      let
        doCheck eidx = first (Map.singleton eidx . pure @[] . Text.pack) .
          mapM_ (uncurry $ checkRef tree defs) . Mos.toList
      in
        either
          (doCheck $ PathError path)
          (smashErrMap .
           Map.mapWithKey (\tpid -> doCheck (TimePointError path tpid)))
          refClaims
    checkRef
      :: MonadFail m => RoseTree [WireValue] -> DefMap -> TypeName -> Path
      -> m ()
    checkRef tree defs requiredTn refP = do
      actualTn <- getTypeAssignment defs refP
      unless (actualTn == requiredTn) $
        fail "Reference points to value of bad type"
      maybe (fail "Reference to missing path") (const $ return ()) $
        Tree.treeLookup refP tree

validateVs
  :: Map Path (Maybe (Set TpId)) -> Valuespace
  -> Either (Map (ErrorIndex TypeName) [Text]) (Map Path TypeName, Valuespace)
validateVs t v = do
    (newTypeAssns, refClaims, vs) <-
      -- As the root type is dynamic we always treat it as if it has been
      -- redefined:
      inner mempty mempty (Map.insert Root Nothing t) v
    checkRefClaims vs refClaims
    let (preExistingXrefs, newXrefs) = partitionXrefs (vsXrefs vs) refClaims
    let existingXrefErrs = validateExistingXrefs preExistingXrefs newTypeAssns
    unless (null existingXrefErrs) $ Left existingXrefErrs
    let vs' = vs {vsXrefs = xrefUnion preExistingXrefs newXrefs}
    return (newTypeAssns, vs')
  where
    errP p = first (Map.singleton (PathError p) . pure . Text.pack)
    tLook p = maybe (Left $ Map.singleton (PathError p) ["not found"]) Right .
      Tree.treeLookup p
    changed :: Eq a => a -> a -> Maybe a
    changed a1 a2 | a1 == a2 = Nothing
                  | otherwise = Just a2

    -- FIXME: this currently bails earlier than it could in light of validation
    -- errors. If we can't figure out the type of children in order to recurse,
    -- then we should probably stop, but if we just encouter bad data, we should
    -- probably just capture the error and continue.
    inner
      :: Map Path TypeName
      -> TypeClaimsByPath
      -> Map Path (Maybe (Set TpId)) -> Valuespace
      -> Either (Map (ErrorIndex TypeName) [Text])
           (Map Path TypeName, TypeClaimsByPath, Valuespace)
    inner newTas newRefClaims tainted vs@(Valuespace tree _ oldTyAssns _) =
      case Map.toAscList tainted of
        [] -> return (newTas, newRefClaims, vs)
        ((path, invalidatedTps):_) -> do
          def <- errP path $ defForPath path vs
          rtn <- tLook path tree
          pathRefClaims <- errP path $
            validateRoseTreeNode def rtn invalidatedTps
          let oldChildTypes = Map.mapMaybe id $ alToMap $ alFmapWithKey
                (\name _ -> Mos.getDependency (path :/ name) oldTyAssns) $
                treeChildren rtn
          let newChildTypes = Map.mapMaybe id $ alToMap $ alFmapWithKey
                (\name _ -> defDispatch (childTypeFor name) def) $
                treeChildren rtn
          let changedChildTypes = merge
                dropMissing preserveMissing
                (zipWithMaybeMatched $ const changed)
                oldChildTypes newChildTypes
          let changedChildPaths = Map.mapKeys (path :/) changedChildTypes
          inner
            (newTas <> changedChildPaths)
            (Map.insert path pathRefClaims newRefClaims)
            (Map.delete path $
               tainted <> fmap (const Nothing) changedChildPaths)
            (vs {vsTyAssns = Mos.setDependencies changedChildPaths oldTyAssns})

opsTouched :: ContainerOps -> DataDigest -> Map Path (Maybe (Set TpId))
opsTouched cops dd = fmap (const Nothing) cops <> fmap classifyDc (alToMap dd)
  where
    classifyDc :: DataChange -> Maybe (Set TpId)
    classifyDc (ConstChange {}) = Nothing
    classifyDc (TimeChange m) = Just $ Map.keysSet m

processToRelayProviderDigest
  :: TrpDigest -> Valuespace
  -> Either (Map (ErrorIndex TypeName) [Text]) (Map Path TypeName, Valuespace)
processToRelayProviderDigest trpd vs =
  let
    ns = trpdNamespace trpd
    tas = foldl removeTamSubtree (vsTyAssns vs) $ trpdRemovedPaths trpd
    redefdPaths = mconcat $
      fmap (\tn -> Mos.getDependants (TypeName ns tn) tas) $ Map.keys $
      trpdDefinitions trpd
    qData = fromJust $ alMapKeys (ns :</) $ trpdData trpd
    qCops = Map.mapKeys (ns :</) $ trpdContainerOps trpd
    updatedPaths = opsTouched qCops qData
    tpRemovals :: DataChange -> Set TpId
    tpRemovals (ConstChange {})= mempty
    tpRemovals (TimeChange m) = Map.keysSet $ Map.filter (isRemove . snd) m
    xrefs' = Map.foldlWithKey' (\x r ts -> removeXrefsTps r ts x) (vsXrefs vs) $
      fmap tpRemovals $ alToMap qData
    (undefOps, defOps) = Map.partition isUndef (trpdDefinitions trpd)
    defs' =
      let
        newDefs = odDef <$> defOps
        updateNsDefs Nothing = Just newDefs
        updateNsDefs (Just existingDefs) = Just $ Map.union newDefs $
          Map.withoutKeys existingDefs (Map.keysSet undefOps)
      in
        Map.alter updateNsDefs ns (vsTyDefs vs)
    (updateErrs, tree') = Tree.updateTreeWithDigest qCops qData (vsTree vs)
  in do
    unless (null updateErrs) $ Left $ Map.mapKeys PathError updateErrs
    (updatedTypes, vs') <- validateVs
      (Map.fromSet (const Nothing) redefdPaths <> updatedPaths) $
      Valuespace tree' defs' (vsTyAssns vs) xrefs'
    return (updatedTypes, vs')

validatePath :: MonadFail m => Valuespace -> Path -> Maybe (Set TpId) -> m ()
validatePath vs p mTpids = do
    def <- defForPath p vs
    t <- note "Missing tree node" $ Tree.treeLookup p $ vsTree vs
    claims <- validateRoseTreeNode def t mTpids
    either (fail . show) return $ checkRefClaims vs $ Map.singleton p claims

processToRelayClientDigest
  :: ContainerOps -> DataDigest -> Valuespace -> Map Path [Text]
processToRelayClientDigest reords dd vs =
  let
    (updateErrs, tree') = Tree.updateTreeWithDigest reords dd (vsTree vs)
    vs' = vs {vsTree = tree'}
    touched = opsTouched reords dd
    touchedLiberties = Map.mapWithKey (\k _ -> getLiberty k vs) touched
    cannotErrs = const ["You touched a cannot"]
      <$> Map.filter (== Just Cannot) touchedLiberties
    mustErrs = const ["You failed to provide a value for must"] <$>
      ( Map.filter (== Just Must)
      $ Map.fromSet (flip getLiberty vs) $ Set.fromList
      $ Tree.treeMissing tree')
    validationErrs = Map.mapWithKey
        (\p mTpids -> either (return . Text.pack) (const []) $ validatePath vs' p mTpids)
        touched
  in
    foldl (Map.unionWith (<>)) updateErrs [
      validationErrs, cannotErrs, mustErrs]

validateRoseTreeNode
  :: MonadFail m
  => Definition -> RoseTree [WireValue] -> Maybe (Set TpId)
  -> m (Either RefTypeClaims (Map TpId RefTypeClaims))
validateRoseTreeNode def t invalidatedTps = case t of
  RtEmpty -> fail "Empty node"
  RtConstData _ wvs -> case def of
    TupleDef (TupleDefinition _ alTreeTypes _) ->
      Left <$> validateWireValues (alValues alTreeTypes) wvs
    _ -> fail "Unexpected constant value!"
  RtDataSeries m -> case def of
    TupleDef (TupleDefinition _ alTreeTypes _) ->
      let toValidate = case invalidatedTps of
            Nothing -> Dkmap.valueMap m
            Just tpids -> Map.restrictKeys (Dkmap.valueMap m) tpids
      in
        fmap Right $
        mapM (validateWireValues (alValues alTreeTypes) . snd . snd) toValidate
    _ -> fail "Unexpected time series data!"
  RtContainer alCont -> case def of
    TupleDef _ -> fail "Y'all have a container where you wanted data"
    StructDef (StructDefinition _ alDef) -> return $ Left mempty
    ArrayDef _ -> return $ Left mempty

validateWireValues
  :: MonadFail m => [TreeType] -> [WireValue] -> m RefTypeClaims
validateWireValues tts wvs =
    (fmtStrictZipError "types" "values" $ strictZipWith vr tts wvs)
    >>= sequence >>= return . Mos.fromList . mconcat
  where
    vr tt wv = validate tt wv >> return (extractTypeAssertion tt wv)

-- FIXME: The VS you get back from this can be invalid WRT refs/inter NS types
vsRelinquish :: Seg -> Valuespace -> Valuespace
vsRelinquish ns (Valuespace tree defs tas xrefs) =
  let
    nsp = Root :/ ns
  in
    Valuespace
      (Tree.treeDelete nsp tree)
      (Map.delete ns defs)
      (Mos.filterDeps
       (\p (TypeName ns' _) -> p `Path.isChildOf` nsp || ns == ns') tas)
      (filterXrefs (\p -> not (p `Path.isChildOf` nsp)) xrefs)

validateExistingXrefs
  :: Xrefs -> Map Path TypeName -> Map (ErrorIndex TypeName) [Text]
validateExistingXrefs xrs newTas =
  let
    retypedPaths = Map.keysSet newTas
    invalidated = Map.restrictKeys xrs retypedPaths
    errText referee = ["Ref target changed type: " <> Path.toText referee]
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

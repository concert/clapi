{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE
    OverloadedStrings
  , QuasiQuotes
  , PatternSynonyms
  , TypeApplications
  , TupleSections
  , FlexibleContexts
  , DataKinds
  , GADTs
#-}

module Clapi.Valuespace
  ( Valuespace, vsTree
  , baseValuespace
  , vsLookupDef, valuespaceGet
  , apiNs, rootTypeName, apiTypeName, dnSeg
  , processToRelayProviderDigest, processToRelayClientDigest
  , validateVs, unsafeValidateVs
  , vsRelinquish, ValidationErr(..)
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
import Data.Maybe (fromJust, mapMaybe)
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
import Clapi.Tree (RoseTree(..), RoseTreeNode, treeInsert, treeChildren, TpId, RoseTreeNodeType(..))
import qualified Clapi.Tree as Tree
import Clapi.Types (WireValue(..))
import Clapi.Types.Base (InterpolationLimit(ILUninterpolated))
import Clapi.Types.AssocList
  ( alKeysSet, alValues, alFromMap, alSingleton, alEmpty
  , unsafeMkAssocList, alMapKeys, alFmapWithKey, alToMap)
import Clapi.Types.Definitions
  ( TreeDefinition(..), ClientPermission(..) , Mandatoriness(..)
  , TupleDefinition(..) , StructDefinition(..), childTypeNameFor
  , childPermissionsFor)
import Clapi.Types.Digests
  ( DefOp(..), isUndef, ContainerOps, DataChange(..), isRemove, DataDigest
  , TrpDigest(..), trpdRemovedPaths)
import Clapi.Types.Messages (ErrorIndex(..))
import Clapi.Types.Path
  (Seg, Path, pattern (:/), pattern Root, pattern (:</))
import qualified Clapi.Types.Path as Path
import Clapi.Types.Tree (TreeType(..), ttWord32, ttInt32, unbounded, ttString, RefTarget)
import Clapi.Types.TypeName
  ( ChildTypeName(..), TypeName(ValueTypeName, TreeTypeName), TypeNamespace(..)
  , AnyTypeName, ctnToAnyTypeName, ctnNamespace, ctnConc, ctnCont, rawTypeName)
import Clapi.Validator (validate, extractTypeAssertion)
import qualified Clapi.Types.Dkmap as Dkmap

type DefinitionMap a = Map Seg (Map Seg a)
type ValDefMap = DefinitionMap TupleDefinition
type TreeDefMap = DefinitionMap TreeDefinition
type CreateDefMap = DefinitionMap (StructDefinition Mandatoriness)

type TypeAssignmentMap = Mos.Dependencies Path (ChildTypeName 'TnTree)
type Referer = Path
type Referee = Path
type Xrefs = Map Referee (Map Referer (Maybe (Set TpId)))

data Valuespace = Valuespace
  { vsTree :: RoseTree [WireValue]
  , vsTreeDefs :: TreeDefMap
  , vsCreateDefs :: CreateDefMap
  , vsValDefs :: ValDefMap
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

apiNs :: Seg
apiNs = [segq|api|]

rootTypeName, apiTypeName :: TypeName 'TnTree
rootTypeName = TreeTypeName apiNs [segq|root|]
apiTypeName = TreeTypeName apiNs apiNs

apiDef :: a -> StructDefinition a
apiDef a = StructDefinition "Information about CLAPI itself" $
  alSingleton [segq|version|] (CtnConc $ ValueTypeName apiNs [segq|version|], a)

versionDef :: TupleDefinition
versionDef = TupleDefinition "The version of CLAPI" (unsafeMkAssocList
  [ ([segq|major|], ttWord32 unbounded)
  , ([segq|minor|], ttWord32 unbounded)
  , ([segq|revision|], ttInt32 unbounded)
  ]) ILUninterpolated

dnSeg :: Seg
dnSeg = [segq|display_name|]

displayNameDef :: TupleDefinition
displayNameDef = TupleDefinition
  "A human-readable name for a struct or array element"
  (alSingleton [segq|name|] $ ttString "") ILUninterpolated

-- | Fully revalidates the given Valuespace and throws an error if there are any
--   validation issues.
unsafeValidateVs :: Valuespace -> Valuespace
unsafeValidateVs vs = either (error . show) snd $ validateVs allTainted vs
  where
    allTainted = Map.fromList $ fmap (,Nothing) $ Tree.treePaths Root $
      vsTree vs

baseValuespace :: Valuespace
baseValuespace = unsafeValidateVs $ Valuespace
    baseTree baseTreeDefs baseCreateDefs baseValDefs baseTas mempty
  where
    vseg = [segq|version|]
    version = RtConstData Nothing
      [WireValue @Word32 0, WireValue @Word32 1, WireValue @Int32 (-1023)]
    baseTree =
      treeInsert Nothing (Root :/ apiNs :/ vseg) version Tree.RtEmpty
    baseValDefs = Map.singleton apiNs $ Map.fromList
      [ (vseg, versionDef), (dnSeg, displayNameDef) ]
    baseTreeDefs = Map.singleton apiNs $
        Map.singleton apiNs $ TStructDef $ apiDef ReadOnly
    baseCreateDefs = mempty
    baseTas = Mos.dependenciesFromMap $ Map.singleton Root $ CtnCont rootTypeName

type TreeDef = Either TupleDefinition TreeDefinition

lookupInDefMap :: MonadFail m => Seg -> Seg -> DefinitionMap a -> m a
lookupInDefMap ns n defs = note "Missing def" $ Map.lookup ns defs >>= Map.lookup n

lookupTreeDef
  :: MonadFail m => TypeName 'TnTree -> TreeDefMap
  -> m TreeDefinition
lookupTreeDef tn@(TreeTypeName ns s) defs = note "Missing def" $
    lookupInDefMap ns s defs <|>
    if tn == rootTypeName then Just rootDef else Nothing
  where
    -- NB: We generate the root def on the fly when people ask about it
    rootDef = TStructDef $ StructDefinition "root def doc" $ alFromMap $
      Map.mapWithKey (\k _ -> (CtnCont $ TreeTypeName k k, ReadOnly)) defs

vsLookupDef
  :: MonadFail m => ChildTypeName 'TnTree -> Valuespace
  -> m TreeDef
vsLookupDef ctn vs = case ctn of
    CtnConc tn -> fmap Left $ uncurry lookupInDefMap (rawTypeName tn) $ vsValDefs vs
    CtnCont tn -> fmap Right $ lookupTreeDef tn $ vsTreeDefs vs

lookupTypeName :: MonadFail m => Path -> TypeAssignmentMap -> m (ChildTypeName 'TnTree)
lookupTypeName p = note "Type name not found" . Mos.getDependency p

defForPath :: MonadFail m => Path -> Valuespace -> m TreeDef
defForPath p vs = lookupTypeName p (vsTyAssns vs) >>= flip vsLookupDef vs

valuespaceGet
  :: MonadFail m => Path -> Valuespace
  -> m
      ( RoseTreeNode [WireValue]
      , ClientPermission
      , ChildTypeName 'TnTree
      , TreeDef
      )
valuespaceGet p vs = do
    rtn <- note "Path not found" $ Tree.treeLookupNode p $ vsTree vs
    tn <- lookupTypeName p $ vsTyAssns vs
    cp <- getPermissions p vs
    def <- vsLookupDef tn vs
    return (rtn, cp, tn, def)

type RefTypeClaims = Mos RefTarget Referee
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
  -> Either (Map (ErrorIndex AnyTypeName) [ValidationErr]) ()
checkRefClaims tyAssns = smashErrMap . Map.mapWithKey checkRefsAtPath
  where
    errIf m = unless (null m) $ Left m
    smashErrMap = errIf . Mol.unions . lefts . Map.elems
    --checkRefsAtPath
    --  :: Path
    --  -> Either RefTypeClaims (Map TpId RefTypeClaims)
    --  -> Either (Map (ErrorIndex a) [ValidationErr]) ()
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
    checkRef :: RefTarget -> Path -> Either ValidationErr ()
    checkRef requiredTn refP = case lookupTypeName refP tyAssns of
        Nothing -> Left $ RefTargetNotFound refP
        Just actualTn -> if actualTn == requiredTn
            then Right ()
            else Left $ RefTargetTypeErr refP actualTn requiredTn

validateVs
  :: Map Path (Maybe (Set TpId)) -> Valuespace
  -> Either (Map (ErrorIndex AnyTypeName) [ValidationErr]) (Map Path RefTarget, Valuespace)
validateVs t v = do
    (newTypeAssns, refClaims, vs) <-
      -- As the root type is dynamic we always treat it as if it has been
      -- redefined:
      inner mempty mempty (Map.insert Root Nothing t) v
    checkRefClaims (vsTyAssns vs) refClaims
    let (preExistingXrefs, newXrefs) = partitionXrefs (vsXrefs vs) refClaims
    let existingXrefErrs = validateExistingXrefs preExistingXrefs newTypeAssns
    unless (null existingXrefErrs) $ Left $ Map.mapKeys (fmap ctnToAnyTypeName) existingXrefErrs
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
      :: Map Path RefTarget
      -> TypeClaimsByPath
      -> Map Path (Maybe (Set TpId)) -> Valuespace
      -> Either (Map (ErrorIndex AnyTypeName) [ValidationErr])
           (Map Path RefTarget, TypeClaimsByPath, Valuespace)
    inner newTas newRefClaims tainted vs =
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
              rtn <- tLook path $ vsTree vs
              case validateRoseTreeNode def rtn invalidatedTps of
                Left validationErrs -> if null emptyArrays
                    then Left $ Map.singleton (PathError path) validationErrs
                    else inner newTas' newRefClaims tainted' vs'
                  where
                    emptyArrays = mapMaybe mHandlable validationErrs
                    isEmptyContainer d = case d of
                        Right (TArrayDef _) -> True
                        Right (TStructDef (StructDefinition _ defKids)) -> defKids == alEmpty
                        _ -> False
                    mHandlable ve = case ve of
                        MissingChild name -> do
                          ctn <- either (const Nothing) (childTypeNameFor name) def
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
                    vs' = vs {vsTree = foldl (\acc (cp, _) -> insertEmpty cp acc) (vsTree vs) qEmptyArrays}
                Right pathRefClaims -> inner
                      (newTas <> changedChildPaths)
                      (Map.insert path pathRefClaims newRefClaims)
                      (Map.delete path $
                         tainted <> fmap (const Nothing) changedChildPaths)
                      (vs {vsTyAssns = Mos.setDependencies changedChildPaths $ vsTyAssns vs})
                  where
                    oldChildTypes = Map.mapMaybe id $ alToMap $ alFmapWithKey
                      (\name _ -> Mos.getDependency (path :/ name) $ vsTyAssns vs) $
                      treeChildren rtn
                    newChildTypes = Map.mapMaybe id $ alToMap $ alFmapWithKey
                      (\name _ -> childTypeName name def) $
                      treeChildren rtn
                    changedChildTypes = merge
                      dropMissing preserveMissing
                      (zipWithMaybeMatched $ const changed)
                      oldChildTypes newChildTypes
                    changedChildPaths = Map.mapKeys (path :/) changedChildTypes

opsTouched :: ContainerOps -> DataDigest -> Map Path (Maybe (Set TpId))
opsTouched cops dd = fmap (const Nothing) cops <> fmap classifyDc (alToMap dd)
  where
    classifyDc :: DataChange -> Maybe (Set TpId)
    classifyDc (ConstChange {}) = Nothing
    classifyDc (TimeChange m) = Just $ Map.keysSet m

applyDefOps :: Seg -> Map Seg (DefOp a) -> DefinitionMap a -> DefinitionMap a
applyDefOps ns ops = Map.alter updateNsDefs ns
  where
    (undefOps, defOps) = Map.partition isUndef ops
    newDefs = odDef <$> defOps
    updateNsDefs Nothing = Just newDefs
    updateNsDefs (Just existingDefs) = Just $ Map.union newDefs $
      Map.withoutKeys existingDefs (Map.keysSet undefOps)

processToRelayProviderDigest
  :: TrpDigest -> Valuespace
  -> Either (Map (ErrorIndex AnyTypeName) [Text]) (Map Path RefTarget, Valuespace)
processToRelayProviderDigest trpd vs =
  let
    ns = trpdNamespace trpd
    tas = foldl removeTamSubtree (vsTyAssns vs) $ trpdRemovedPaths trpd
    redefdPaths = mconcat $
      fmap (\ctn -> Mos.getDependants ctn tas)
        $ (fmap (ctnCont ns) $ Map.keys $ trpdTreeDefs trpd)
        ++ (fmap (ctnConc ns) $ Map.keys $ trpdValueDefs trpd)
    qData = fromJust $ alMapKeys (ns :</) $ trpdData trpd
    qCops = Map.mapKeys (ns :</) $ trpdContainerOps trpd
    updatedPaths = opsTouched qCops qData
    tpRemovals :: DataChange -> Set TpId
    tpRemovals (ConstChange {})= mempty
    tpRemovals (TimeChange m) = Map.keysSet $ Map.filter (isRemove . snd) m
    xrefs' = Map.foldlWithKey' (\x r ts -> removeXrefsTps r ts x) (vsXrefs vs) $
      fmap tpRemovals $ alToMap qData
    treeDefs' = applyDefOps ns (trpdTreeDefs trpd) $ vsTreeDefs vs
    createDefs' = applyDefOps ns (trpdCreateDefs trpd) $ vsCreateDefs vs
    valDefs' = applyDefOps ns (trpdValueDefs trpd) $ vsValDefs vs
    (updateErrs, tree') = Tree.updateTreeWithDigest qCops qData (vsTree vs)
  in do
    unless (null updateErrs) $ Left $ Map.mapKeys PathError updateErrs
    (updatedTypes, vs') <- first (fmap $ fmap $ Text.pack . show) $ validateVs
      (Map.fromSet (const Nothing) redefdPaths <> updatedPaths) $
      Valuespace tree' treeDefs' createDefs' valDefs' tas xrefs'
    return (updatedTypes, vs')

validatePath :: Valuespace -> Path -> Maybe (Set TpId) -> Either [ValidationErr] (Either RefTypeClaims (Map TpId RefTypeClaims))
validatePath vs p mTpids = do
    def <- first pure $ fromMonadFail $ defForPath p vs
    t <- maybe (Left [ProgrammingErr "Tainted but missing"]) Right $ Tree.treeLookup p $ vsTree vs
    validateRoseTreeNode def t mTpids

getPermissions :: MonadFail m => Path -> Valuespace -> m ClientPermission
getPermissions path vs = case path of
    Root :/ _ -> return ReadOnly
    p :/ s -> defForPath p vs >>= either
        (const $ fail "Child of value")
        (note "Parent doesn't recognise child" . childPermissionsFor s)
    _ -> return ReadOnly

processToRelayClientDigest
  :: ContainerOps -> DataDigest -> Valuespace -> Map (ErrorIndex AnyTypeName) [ValidationErr]
processToRelayClientDigest reords dd vs =
  let
    (updateErrs, tree') = Tree.updateTreeWithDigest reords dd (vsTree vs)
    touched = opsTouched reords dd
    (tas', newPaths) = fillTyAssns (vsTreeDefs vs) (vsTyAssns vs) (Map.keys touched)
    -- FIXME: Are you allowed to set things you just created?
    vs' = vs {vsTree = tree', vsTyAssns = tas'}
    touchedPermissions = Map.mapWithKey (\k _ -> getPermissions k vs') touched
    cannotErrs = const [PermissionsErr "Modified something readonly"]
      <$> Map.filter (== Just ReadOnly) touchedPermissions
    (validationErrs, refClaims) = Map.mapEitherWithKey (validatePath vs') touched
    refErrs = either id (const mempty) $ checkRefClaims (vsTyAssns vs') refClaims
  in
    foldl (Map.unionWith (<>)) refErrs $ fmap (Map.mapKeys PathError)
      [fmap (fmap $ GenericErr . Text.unpack) updateErrs, validationErrs, cannotErrs]

childTypeName
  :: Seg -> Either TupleDefinition TreeDefinition
  -> Maybe (ChildTypeName 'TnTree)
childTypeName cSeg = either (const Nothing) (childTypeNameFor cSeg)

fillTyAssns :: TreeDefMap -> TypeAssignmentMap -> [Path] -> (TypeAssignmentMap, Set Path)
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
            onlyCtnCont ctn = case ctn of
                CtnCont tn -> return tn
                _ -> fail "Not a container"
            mChildTn = mptn >>= onlyCtnCont >>= flip lookupTreeDef defs >>= childTypeNameFor cSeg
          in case mChildTn of
            Just tn -> Map.insert p tn tm'
            Nothing -> tm'

data ValidationErr
  = GenericErr String
  | ProgrammingErr String
  | BadNodeType {vebntExpected :: RoseTreeNodeType, vebntActual :: RoseTreeNodeType}
  | MissingChild Seg
  | ExtraChild Seg
  | RefTargetNotFound Path
  | RefTargetTypeErr {veRttePath :: Path, veRtteExpectedType :: RefTarget, veRtteTargetType :: RefTarget}
  | PermissionsErr String
  deriving (Show)

fromMonadFail :: Either String a -> Either ValidationErr a
fromMonadFail mf = case mf of
    Left msg -> Left $ GenericErr msg
    Right a -> Right a

defNodeType :: TreeDef -> RoseTreeNodeType
defNodeType def = case def of
    Right _ -> RtntContainer
    Left (TupleDefinition _ _ interpLim) -> case interpLim of
        ILUninterpolated -> RtntConstData
        _ -> RtntDataSeries

validateRoseTreeNode
  :: TreeDef -> RoseTree [WireValue] -> Maybe (Set TpId)
  -> Either [ValidationErr] (Either RefTypeClaims (Map TpId RefTypeClaims))
validateRoseTreeNode def t invalidatedTps = case t of
    RtEmpty -> tyErr
    RtConstData _ wvs -> case def of
      Left (TupleDefinition _ alTreeTypes _) -> first pure $ fromMonadFail $
        Left <$> validateWireValues (alValues alTreeTypes) wvs
      _ -> tyErr
    RtDataSeries m -> case def of
      Left (TupleDefinition _ alTreeTypes _) ->
        let toValidate = case invalidatedTps of
              Nothing -> Dkmap.valueMap m
              Just tpids -> Map.restrictKeys (Dkmap.valueMap m) tpids
        in first pure $ fromMonadFail $
          fmap Right $
          mapM (validateWireValues (alValues alTreeTypes) . snd . snd) toValidate
      _ -> tyErr
    RtContainer alCont -> case def of
      Left _ -> tyErr
      Right (TStructDef (StructDefinition _ alDef)) -> if defSegs == rtnSegs
          then return $ Left mempty
          else Left $ fmap MissingChild missingSegs ++ fmap ExtraChild extraSegs
        where
          defSegs = alKeysSet alDef
          rtnSegs = alKeysSet alCont
          missingSegs = Set.toList $ Set.difference defSegs rtnSegs
          extraSegs = Set.toList $ Set.difference rtnSegs defSegs
      Right (TArrayDef _) -> return $ Left mempty
  where
    tyErr = Left [BadNodeType (defNodeType def) (Tree.rtType t)]

validateWireValues
  :: MonadFail m => [TreeType] -> [WireValue] -> m RefTypeClaims
validateWireValues tts wvs =
    (fmtStrictZipError "types" "values" $ strictZipWith vr tts wvs)
    >>= sequence >>= return . Mos.fromList . mconcat
  where
    vr tt wv = validate tt wv >> return (extractTypeAssertion tt wv)

-- FIXME: The VS you get back from this can be invalid WRT refs/inter NS types
vsRelinquish :: Seg -> Valuespace -> Valuespace
vsRelinquish ns (Valuespace tree valDefs treeDefs createDefs tas xrefs) =
  let
    nsp = Root :/ ns
  in
    Valuespace
      (Tree.treeDelete nsp tree)
      (Map.delete ns valDefs)
      (Map.delete ns treeDefs)
      (Map.delete ns createDefs)
      (Mos.filterDeps
       (\p ctn -> not $ p `Path.isChildOf` nsp || ns == ctnNamespace ctn) tas)
      (filterXrefs (\p -> not (p `Path.isChildOf` nsp)) xrefs)

validateExistingXrefs
  :: Xrefs -> Map Path RefTarget -> Map (ErrorIndex RefTarget) [ValidationErr]
validateExistingXrefs xrs newTas =
  let
    retypedPaths = Map.keysSet newTas
    invalidated = Map.restrictKeys xrs retypedPaths
    errText referee = [GenericErr $ "Ref target changed type: " ++ Text.unpack (Path.toText referee)]
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

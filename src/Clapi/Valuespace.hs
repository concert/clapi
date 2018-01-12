{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Rank2Types #-}

module Clapi.Valuespace where

import Prelude hiding (fail)
import Control.Monad (liftM, when, (>=>), void, join)
import Control.Monad.Fail (MonadFail, fail)
import Control.Monad.Except (throwError, runExceptT)
import Control.Monad.State (State, modify, get, runState)
import Control.Lens ((&), makeLenses, makeFields, view, at, over, set, non, _Just, _1, _2)
import Data.Either (isLeft)
import Data.Int
import Data.Maybe (isJust, fromJust, mapMaybe, catMaybes)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map.Strict.Merge (
    merge, zipWithMaybeMatched, dropMissing, preserveMissing)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word
import Text.Printf (printf)

import Data.Attoparsec.Text (parseOnly)

import qualified Data.Map.Mos as Mos

import Data.Proxy
import Data.Maybe.Clapi (note)
import qualified Data.Maybe.Clapi as Maybe

import Clapi.Util
  ( duplicates, partitionDifferenceL, strictZip, strictZipWith
  , fmtStrictZipError)
import Clapi.Path (Path, pattern (:/))
import qualified Clapi.Path as Path
import Clapi.Types (
    CanFail, InterpolationType(..), Interpolation(..), Time(..),
    interpolationType, Site, Attributee,
    WireValue(..), Wireable, (<|$|>), (<|*|>))
import Clapi.Tree (
    ClapiTree, NodePath, TypePath, Attributed,
    TimePoint, SiteMap, treeInitNode, treeDeleteNode, treeAdd, treeSet,
    treeRemove, treeClear, treeSetChildren, getKeys, getSites, unwrapTimePoint,
    getChildPaths, TreeDelta, treeDiff)
import qualified Clapi.Tree as Tree
import Clapi.Types.AssocList
  (mkAssocList, alFromZip, unAssocList, alKeys, alValues, alLookup)
import Clapi.Types.Definitions
  ( MetaType(..), OfMetaType, Definition(..), Liberty(..), TupleDefinition(..)
  , StructDefinition(..), ArrayDefinition(..), defDispatch, childTypeFor
  , valuesToDef, toWireValues, metaType)
import Clapi.Types.Tree
  ( TreeType(..), TreeConcreteType(..), TreeContainerType(..)
  , TreeContainerTypeName, ttEnum , unbounded)
import Clapi.TextSerialisation (ttFromText, ttToText)
import Clapi.Validator (validate, unpackTreeType)
import Clapi.PathQ
import Clapi.Types.AssocList (AssocList)

type Node = Tree.Node [WireValue]

fromLeft :: Either a b -> a
fromLeft (Left a) = a

fromRight :: Either a b -> b
fromRight (Right b) = b

mapFilterJust :: Map.Map k (Maybe a) -> Map.Map k a
mapFilterJust = fmap fromJust . Map.filter isJust

metaTypePath :: MetaType -> Path
metaTypePath Tuple = [pathq|/api/types/base/tuple|]
metaTypePath Struct = [pathq|/api/types/base/struct|]
metaTypePath Array = [pathq|/api/types/base/array|]

metaTypePaths :: [(Path, MetaType)]
metaTypePaths = [(metaTypePath mt, mt) | mt <- [minBound..]]

metaTypePathFor :: MonadFail m => TypePath -> m MetaType
metaTypePathFor mtp = maybe (fail $ "Weird metapath: " ++ show mtp) return $
  lookup mtp metaTypePaths

isMetaTypePath :: TypePath -> Bool
isMetaTypePath = maybe False (const True) . metaTypePathFor

libTt, docTt, segTt, defTt :: TreeType
libTt = ttEnum (Proxy :: Proxy Liberty)
docTt = TtConc $ TcString ""
segTt = TtConc $ TcString "[A-z0-9_]+"
defTt = TtConc $ TcRef [pathq|/api/types/base|]

baseTupleDef = TupleDefinition
    "Base tuple definition"
    (fromJust $ mkAssocList [
        ([segq|doc|], docTt),
        ([segq|valueNames|], TtCont $ TcOrdSet segTt),
        ([segq|validators|], TtCont $ TcList $ TtConc TcValidatorDesc),
        ([segq|interpolationTypes|], TtCont $ TcSet $ ttEnum (Proxy :: Proxy InterpolationType))])
    mempty
baseStructDef = TupleDefinition
    "Base struct definition"
    (fromJust $ mkAssocList
      [ ([segq|doc|], docTt)
      , ([segq|childNames|], TtCont $ TcOrdSet segTt)
      , ([segq|childTypes|], TtCont $ TcList defTt)
      , ([segq|clibs|], TtCont $ TcList libTt)])
    mempty
baseArrayDef = TupleDefinition
    "Base array definition"
    (fromJust $ mkAssocList
      [ ([segq|doc|], docTt)
      , ([segq|childType|], defTt)
      , ([segq|clib|], libTt)])
    mempty


type VsTree = ClapiTree [WireValue]
type DefMap = Map.Map NodePath Definition
type Validated = ()
type TaintTracker = Map.Map NodePath (Maybe (Set.Set (Maybe Site, Time)))
type ExplicitTypeAssignments = Set.Set TypePath
data OwnerUnvalidated = OwnerUnvalidated {_ownerUnvalidatedUvtt :: TaintTracker, _ownerUnvalidatedEtas :: ExplicitTypeAssignments}
data ClientUnvalidated = ClientUnvalidated {_clientUnvalidatedUvtt :: TaintTracker, origVs :: Valuespace Validated}
data Valuespace v = Valuespace {
    _tree :: VsTree,
    _types :: Mos.Dependencies NodePath TypePath,
    _xrefs :: Mos.Dependencies' (NodePath, (Maybe Site, Time)) NodePath,
    _defs :: DefMap,
    _unvalidated :: v
    } deriving (Eq)
makeFields ''OwnerUnvalidated
makeFields ''ClientUnvalidated
makeLenses ''Valuespace

ownerUnlock :: Valuespace Validated -> Valuespace OwnerUnvalidated
ownerUnlock (Valuespace tr ty x d _) = Valuespace tr ty x d (OwnerUnvalidated mempty mempty)

clientUnlock :: Valuespace Validated -> Valuespace ClientUnvalidated
clientUnlock vs@(Valuespace tr ty x d _) = Valuespace tr ty x d $ ClientUnvalidated mempty vs

vsGetTree :: Valuespace v -> VsTree
vsGetTree = view tree

type VsDelta = (NodePath, Either TypePath (TreeDelta [WireValue]))

vsDiff :: Valuespace v -> Valuespace w -> [VsDelta]
vsDiff v0 v1 = (map taAsVd typeAssigns) ++ (tdAsVd nd)
  where
    nd = treeDiff (vsGetTree v0) (vsGetTree v1)
    tdAsVd (Right tds) = map (\(p, td) -> (p, Right td)) tds
    taAsVd (p, ta) = (p, Left ta)
    typeAssigns = Map.assocs $ merge
        (dropMissing)
        (preserveMissing)
        (zipWithMaybeMatched (\k t0 t1 -> if t0 == t1 then Nothing else Just t1))
        (fst $ _types v0)
        (fst $ _types v1)

instance Show (Valuespace v) where
    show = show . view tree

instance Monoid v => Monoid (Valuespace v) where
    mempty = Valuespace mempty mempty mempty mempty mempty
    mappend (Valuespace a1 b1 c1 d1 e1) (Valuespace a2 b2 c2 d2 e2) =
        Valuespace (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2)

-- FIXME: This thing's args are backwards
getType :: (MonadFail m) => Valuespace v -> NodePath -> m TypePath
getType vs np = note f . Mos.getDependency np . view types $ vs
  where
    f = printf "No type path specified for %s" (show np)

getDef :: (MonadFail m) => NodePath -> Valuespace v -> m Definition
getDef np vs =
  do
    tp <- getType vs np
    note f $ view (defs . at tp) vs
  where
    f = printf "No cached type definition for %s" (show np)

getNode :: (MonadFail m) => NodePath -> Valuespace v -> m Node
getNode np = note f . view (tree . at np)
  where
    f = printf "No node data found at %s" (show np)

getChildren :: (MonadFail m) => NodePath -> Valuespace v -> m [Path.Seg]
getChildren np vs = getNode np vs >>= return . view getKeys

type ErrorMap = Map.Map NodePath String
type MonadErrorMap a = (ErrorMap, a)

eitherErrorMap ::
    Map.Map NodePath (Either String a) -> MonadErrorMap (Map.Map NodePath a)
eitherErrorMap =
    over _1 (fmap fromLeft) .
    over _2 (fmap fromRight) .
    Map.partition isLeft

vsValidate :: Valuespace OwnerUnvalidated -> MonadErrorMap (Valuespace Validated)
vsValidate vs =
    rectifyTypes vs (view (unvalidated . etas) vs) >>= validateChildren >>= validateData >>= validateXRefs >>= markValid
  where
    markValid = return . set unvalidated ()

rectifyTypes ::
    (HasUvtt v TaintTracker) =>
    Valuespace v -> Set.Set NodePath -> MonadErrorMap (Valuespace v)
rectifyTypes valSpace explicitPaths =
    populateDefsFor (Map.keysSet (view (unvalidated . uvtt) valSpace)) valSpace
  where
    explicitlyAssigned = Map.restrictKeys (fst $ view types valSpace) explicitPaths
    populateDefsFor deflessNodes vs = if null deflessNodes then (mempty, vs) else let
        np = head $ Set.toList deflessNodes
      in do
        case runState (runExceptT $ ensureDeffed np) (deflessNodes, vs) of
            (Left err, (deflessNodes', vs')) -> (Map.fromList [(np, err)], vs')
            (Right _, (deflessNodes', vs')) -> populateDefsFor deflessNodes' vs'
    getOrInferType np inProgress = do
        (dirtyPaths, vs) <- get
        tp <- if Set.member np dirtyPaths
            then maybe
                (parentDerivedType np $ Set.insert np inProgress)
                return
                (Map.lookup np explicitlyAssigned)
            else cfet $ getType vs np
        modify $ \(dirtyPaths, vs) -> (dirtyPaths, over types (Mos.setDependency np tp) vs)
        return tp
    parentDerivedType np inProgress = case np of
        Path.Root -> return $ error "Hit root deriving parent types, code bad."
        (pp :/ cn) -> do
            tp <- getOrInferType pp inProgress
            when (Set.member tp inProgress) $ throwError $
                "Cannot infer type of " ++ show np ++
                " as the meta type of its parent's type " ++ show tp ++
                " requires this to be resolved."
            def <- getOrBuildDef tp inProgress
            cfet $ note ("Parent " ++ show tp ++ " has no child " ++ show cn) $
              defDispatch childTypeFor def cn
    getOrBuildDef tp inProgress = do
        (dirtyPaths, vs) <- get
        if Set.member tp dirtyPaths
          then do
            mt <- getOrInferType tp inProgress >>= cfet . metaTypePathFor
            tn <- get >>= cfet . getNode tp . snd
            md <- cfet $ validateTypeNode mt tn
            modify $ \(dirtyPaths, vs) -> let
                vs' = over defs (Map.insert tp md) vs
                dirtyPaths' = Set.delete tp dirtyPaths
              in
                (dirtyPaths', vs')
            return md
          else cfet $ note "Clean type path defless" $ view (defs . at tp) vs
    ensureDeffed np = do
        tp <- getOrInferType np mempty
        when (isMetaTypePath tp) $ void $ getOrBuildDef np mempty
        modify $ \(dirtyPaths, vs) -> (Set.delete np dirtyPaths, vs)
    cfet cf = case cf of
        Left err -> throwError err
        Right v -> return v

validateTypeNode :: (MonadFail m) => MetaType -> Node -> m Definition
validateTypeNode metaType node =
  let siteMap = view getSites node in
  do
    when (Map.size siteMap /= 1) $ fail "Type nodes must only have global site"
    globalTimeSeries <- note "Missing global time series" $
        Map.lookup Nothing siteMap
    when (Map.size globalTimeSeries /= 1) $
        fail "Type nodes must only have a single time point"
    defCvs <- fmap snd . unwrapTimePoint . snd . head . Map.toList $ globalTimeSeries
    valuesToDef metaType defCvs

taintedNodesWithDefs :: (HasUvtt v TaintTracker) =>
    Valuespace v -> MonadErrorMap (Map.Map NodePath (Node, Definition))
taintedNodesWithDefs vs = eitherErrorMap $ Map.mapWithKey pairDef unvalidatedNodes
  where
    unvalidatedNodes = Map.restrictKeys (view tree vs) (Map.keysSet $
        view (unvalidated . uvtt) vs)
    pairDef np n = (,) n <$> getDef np vs

validateChildren ::
    Valuespace OwnerUnvalidated -> MonadErrorMap (Valuespace OwnerUnvalidated)
validateChildren vs = do
    tnwd <- taintedNodesWithDefs vs
    eitherErrorMap $ Map.mapWithKey
        (\np (n, d) -> validateNodeChildren (getType vs) np n d) tnwd
    return vs


validateChildKeyTypes ::
    (MonadFail m) => (NodePath -> m TypePath) -> NodePath ->
    [Path.Seg] -> (Path.Seg -> Maybe TypePath) -> m ()
validateChildKeyTypes getType' np expectedNames nameToType =
  let
    expectedTypeMap = Map.fromList $
        (\cn -> (cn, (fromJust $ nameToType cn, np :/ cn))) <$> expectedNames
    failTypes bad = when (not . null $ bad) $ fail $
        printf "bad child types for %s:%s" (show np) (concatMap fmtBct $ Map.assocs bad)
    fmtBct :: (Path.Seg, (Path, Path)) -> String
    fmtBct (p, (ap, ep)) = printf " (%s: %s != %s)" (show p) (show ap) (show ep)
  in do
    taggedBadTypes <-
        fmap (Map.filter (uncurry (/=))) $
        (traverse . traverse) getType' expectedTypeMap
    failTypes taggedBadTypes

validateNodeChildren ::
    (MonadFail m) => (NodePath -> m TypePath) -> NodePath -> Node ->
    Definition -> m ()
validateNodeChildren _ _ node (TupleDef {}) = case view getKeys node of
  [] -> return ()
  _ -> fail "tuple node has children"
validateNodeChildren getType' np node (StructDef def) =
  let
    expectedKeys = alKeys $ strDefTypes def
    nodeKeys = view getKeys node
    (missing, extra) = partitionDifferenceL expectedKeys nodeKeys
    failKeys = fail $
        printf "expected node keys %s, got %s" (show expectedKeys)
        (show nodeKeys)
  in do
    when ((missing, extra) /= mempty) failKeys
    validateChildKeyTypes getType' np expectedKeys (childTypeFor def)
validateNodeChildren getType' np node (ArrayDef def) =
  let
    nodeKeys = view getKeys node
    dups = duplicates nodeKeys
    failDups = when (not . null $ dups) $ fail $
        printf "duplicate array keys %s" (show dups)
  in do
    failDups
    validateChildKeyTypes getType' np nodeKeys (childTypeFor def)


flattenNestedMaps ::
    (Ord k1, Ord k2) => Map.Map k1 (Map.Map k2 a) -> Map.Map (k1, k2) a
flattenNestedMaps mm = foldMap id $ Map.mapWithKey f mm
  where
    f k1 = Map.mapKeys ((,) k1)

validateData ::
    (HasUvtt v TaintTracker) => Valuespace v -> MonadErrorMap (Valuespace v)
validateData vs =
  do
    overUnvalidatedNodes (validateNodeData vs) vs
    return vs

validateXRefs ::
    (HasUvtt v TaintTracker) => Valuespace v -> MonadErrorMap (Valuespace v)
validateXRefs vs =
  do
    newXRefDeps <- overUnvalidatedNodes validateXrefsMappy vs
    return $ over xrefs (f newXRefDeps) vs
  where
    f newXRefDeps unv =
        Map.foldrWithKey Mos.setDependencies' unv
        (flattenNestedMaps newXRefDeps)
    validateXrefsMappy :: NodePath -> Map.Map (Maybe Site, Time) (Interpolation, [WireValue]) ->
       CanFail (Map.Map (Maybe Site, Time) [NodePath])
    -- pure below is a fudge to get the values into the (potentially more
    -- efficient) signature of validateXrefsAtPath
    validateXrefsMappy np m = mapM (validateXrefsAtPath np . pure . snd) m
    validateXrefsAtPath :: MonadFail m => NodePath -> [[WireValue]] -> m [NodePath]
    validateXrefsAtPath p wvss = do
      let tts = treeTypesFor p
      mtlonp <- mapM (fmtStrictZipError "types" "values" . strictZipWith extractRefs tts) wvss
      let rvTups = catMaybes $ mconcat mtlonp
      mapM validateRefValues rvTups
      return $ mconcat $ snd <$> rvTups
    treeTypesFor :: NodePath -> [TreeType]
    treeTypesFor p = case getDef p vs of
      Just (TupleDef (TupleDefinition _ al _)) -> alValues al
      _ -> []
    extractRefs :: TreeType -> WireValue -> Maybe (TypePath, [NodePath])
    extractRefs tt wv = let (concT, contTs) = unpackTreeType tt in
      case concT of
        TcRef p -> Just (p, maybe
          (error "Expected ref did not parse second time") id $
            collectRefs contTs (fmap pure . Path.fromText) wv)
        _ -> Nothing
    collectRefs :: (MonadFail m, Wireable a) => [TreeContainerTypeName] -> (a -> m [NodePath]) -> WireValue -> m [NodePath]
    collectRefs [] f wv = join $ f <|$|> wv
    collectRefs (_:cts) f wv = collectRefs cts (fmap mconcat . mapM f) wv
    validateRefValues :: MonadFail m => (TypePath, [NodePath]) -> m ()
    validateRefValues (tp, nps) = do
      ntps <- mapM (getType vs) nps
      case filter (not . Path.isParentOf tp) ntps of
        [] -> return ()
        badNtps -> fail $ "format the argument better " ++ show badNtps

-- This doesn't need to repack the values back into a full SiteMap :-)
-- ... but that does make the abstraction leaky, which is more clear with
-- filterNode
filterSiteMap ::
    SiteMap a -> Set.Set (Maybe Site, Time) ->
    Map.Map (Maybe Site, Time) (Interpolation, a)
filterSiteMap sm keys = mapFilterJust $
    unwrapTimePoint <$> Map.restrictKeys (flattenNestedMaps sm) keys

filterNode ::
    Maybe (Set.Set (Maybe Site, Time)) -> Node ->
    Map.Map (Maybe Site, Time) (Interpolation, [WireValue])
filterNode mtps n =
  let
    sites = view getSites n
    tps = maybe allTps id mtps
    allTps = Map.keysSet . flattenNestedMaps $ sites
  in
    filterSiteMap sites tps

overUnvalidatedNodes ::
    (HasUvtt v TaintTracker) => (
       NodePath -> Map.Map (Maybe Site, Time) (Interpolation, [WireValue]) ->
       CanFail a
    ) -> Valuespace v -> MonadErrorMap (Map.Map NodePath a)
overUnvalidatedNodes f vs =
  do
    filtered <-
        eitherErrorMap . Map.mapWithKey getAndFilterNode $ view (unvalidated . uvtt) vs
    eitherErrorMap . Map.mapWithKey f $ filtered
  where
    getAndFilterNode np mtps = getNode np vs >>= return . filterNode mtps


validateInterpolation ::
    (MonadFail m) => Set.Set InterpolationType -> Interpolation -> m ()
validateInterpolation its i = return ()
-- FIXME: temporarily disabled because of structural inadequacy in tree!
-- validateInterpolation its i | it `elem` its = return ()
--                             | otherwise = fail f
--   where
--     it = interpolationType i
--     f =  printf "forbidden interpolation type %s (allowed %s)"
--         (show it) (show its)


validateNodeData ::
    (MonadFail m) =>
    Valuespace v -> NodePath ->
    Map.Map (Maybe Site, Time) (Interpolation, [WireValue]) -> m ()
validateNodeData vs np siteMap = getDef np vs >>= body
  where
    -- FIXME: we want to hand back multiple errors for a node, probably by
    -- calling partially applied error message constructors when errors occur,
    -- and storing them as a list, rather than using MonadFail that can only
    -- error with one specific string.
    -- Right now, however, we just fail fast because we've got too much WIP.
    body (TupleDef def) = do
        let its = tupDefInterpTypes def
        mapM_ (validateInterpolation its . fst) siteMap
        let treeTypes = alValues $ tupDefTypes def
        fnarf <- mapM (fmtStrictZipError "types" "values" . strictZipWith validate treeTypes . snd) $ Map.elems siteMap
        void $ sequence $ join fnarf
    body _ | siteMap == mempty = return mempty
           | otherwise = fail "data found in container"

-- vsUpdateXRefs :: Valuespace v -> MonadErrorMap (Valuespace Validated)
-- vsUpdateXRefs vs =
--     eitherErrorMap . Map.mapWithKey getXRefDeps $ unvalidatedNodes vs
--   where
--     getXRefDeps np n = do
--         def <- getDef np vs
--         traverse (validate (getType vs) (view validators def)) (toList n)

vsClientValidate :: Valuespace ClientUnvalidated -> MonadErrorMap [VsDelta]
vsClientValidate vs = vsDiff ovs <$> doValidate mempty vs
  where
    ovs = origVs $ view unvalidated vs
    doValidate knownErrs vs = let (em, vs') = doValidateStep vs in if null em
        then (knownErrs, vs')
        else doValidate (Map.union knownErrs em) $ rollbackErrs em vs'
    doValidateStep :: Valuespace ClientUnvalidated -> MonadErrorMap (Valuespace ClientUnvalidated)
    doValidateStep vs = rectifyTypes vs mempty >>= checkLibertiesPermit >>= validateClientChildren >>= validateData
    rollbackErrs errs vs = foldl rollbackPath vs $ Map.keys errs
    rollbackPath vs p = dupNode p (getType ovs p) (Map.lookup p $ vsGetTree ovs) (fromJust $ vsDelete p vs)
    dupNode p (Just tp) (Just n) vs = over tree (Map.insert p n) $ over types (Mos.setDependency p tp) vs
    dupNode p Nothing Nothing vs = maybe vs id $ vsDelete p vs

getLiberty :: MonadFail m => Valuespace ClientUnvalidated -> NodePath -> m Liberty
getLiberty vs p = do
    (parent, seg) <- case p of
        Path.Root -> fail "Liberty of root?"
        (p :/ s) -> return (p, s)
    def <- getDef parent vs
    libertyOf seg def
  where
    libertyOf seg (TupleDef _) = fail "Tuples have no children"
    libertyOf seg (StructDef (StructDefinition _ tyInfo)) =
        note "Not a permitted child of parent struct" $
        fmap snd . alLookup seg $ tyInfo
    libertyOf seg (ArrayDef (ArrayDefinition _ _ clib)) = return clib

checkLibertiesPermit :: Valuespace ClientUnvalidated -> MonadErrorMap (Valuespace ClientUnvalidated)
checkLibertiesPermit vs = (errs, vs)
  where
    taintedPaths = Map.keys $ view (unvalidated . uvtt) vs
    liberties = zip taintedPaths (map (getLiberty vs) taintedPaths :: [Maybe Liberty])
    libErrs = filter (maybe True (== Cannot) . snd) liberties
    errs = Map.fromList $ fmap (fmap libErrString) libErrs
    libErrString l = "Client not permitted to change path with liberty: " ++ (show l)

validateClientChildren :: Valuespace ClientUnvalidated -> MonadErrorMap (Valuespace ClientUnvalidated)
validateClientChildren vs = do
    tnwd <- taintedNodesWithDefs vs
    mapM_ checkPath $ Map.assocs tnwd
    return vs
  where
    checkPath (np, (node, def)) = (validateClientNodeChildren getOldNode getNewNode np node def, ())
    getOldNode np = Map.lookup np $ vsGetTree vs
    getNewNode np = Map.lookup np $ vsGetTree $ origVs $ view unvalidated vs

validateClientNodeChildren
  :: (NodePath -> Maybe Node) -> (NodePath -> Maybe Node) -> NodePath -> Node
  -> Definition -> ErrorMap
validateClientNodeChildren getOldNode getNewNode np node def =
  Map.fromList $ case def of
    (TupleDef _) -> if null childKeys
      then mempty
      else map (\p -> (p, "Tuples have no children!")) childPaths
    (StructDef (StructDefinition _ tyInfo)) -> let
        nameLibMap = Map.fromList $ unAssocList $ fmap snd tyInfo
        getChildErr name = let sp = childPath name in
            fmap ((,) sp) $ case Map.lookup name nameLibMap of
                Nothing -> Just "Unexpected child"
                (Just Cannot) -> if unmodified sp then Nothing else Just "Can't touch"
                (Just Must) -> if null $ getNewNode sp then Just "Must exist" else Nothing
                (Just May) -> Nothing
      in mapMaybe getChildErr childKeys
    (ArrayDef (ArrayDefinition _ _ childLiberty)) ->
      if childLiberty /= Cannot then mempty else
        (\p -> (p, "Not mutable")) <$> filter unmodified childPaths
  where
    childKeys = view getKeys node
    childPath name = np :/ name
    childPaths = map childPath childKeys
    unmodified p = getOldNode p == getNewNode p

up :: Path -> Path
up Path.Root = Path.Root
up (p :/ s) = p

vsAssignType :: (HasUvtt v TaintTracker, HasEtas v ExplicitTypeAssignments) => NodePath -> TypePath -> Valuespace v -> Valuespace v
vsAssignType np tp =
    over tree (treeInitNode np) .
    over types (Mos.setDependency np tp) .
    set (unvalidated . uvtt . at np) (Just Nothing) .
    set (unvalidated . uvtt . at (up np)) (Just Nothing) .
    over (unvalidated . etas) (Set.insert np) .
    taintXRefDependants np .
    taintImplicitAdditions np

vsDelete ::
    (MonadFail m, HasUvtt v TaintTracker) => NodePath -> Valuespace v -> m (Valuespace v)
vsDelete np =
    tree (treeDeleteNode np) .
    over types (Mos.delDependency np) .
    set (unvalidated . uvtt . at np) Nothing .
    set (unvalidated . uvtt . at (up np)) (Just Nothing)

taintData ::
    (HasUvtt v TaintTracker) => NodePath -> Maybe Site -> Time -> Valuespace v -> Valuespace v
taintData np s t =
    over (unvalidated . uvtt . at np . non mempty . non mempty) (Set.insert (s, t))

taintTypeDependants :: (HasUvtt v TaintTracker) => NodePath -> Valuespace v -> Valuespace v
taintTypeDependants np vs =
    over (unvalidated . uvtt) (Maybe.update Map.union maybeTaintedMap) vs
  where
    maybeTaintedMap = Map.fromSet (const Nothing) <$>
        (Mos.getDependants np $ view types vs)

taintXRefDependants :: (HasUvtt v TaintTracker) => NodePath -> Valuespace v -> Valuespace v
taintXRefDependants np vs = over (unvalidated . uvtt) (Map.union taintedMap) vs
  where
    taintedMap =
        fmap Just . Set.foldr (uncurry Mos.insert) mempty .
        Mos.getDependants' np $ view xrefs vs

-- FIXME: Duplicates logic from Tree (about constructing parent nodes)
taintImplicitAdditions :: (HasUvtt v TaintTracker) => NodePath -> Valuespace v -> Valuespace v
taintImplicitAdditions np vs = let pp = up np in if pp /= np && Map.notMember pp (view tree vs)
  then taintImplicitAdditions pp $ set (unvalidated . uvtt . at pp) (Just Nothing) vs
  else vs

vsAdd ::
    (MonadFail m, HasUvtt v TaintTracker) =>
    Maybe Attributee -> Interpolation -> [WireValue] -> NodePath ->
    Maybe Site -> Time -> Valuespace v -> m (Valuespace v)
vsAdd a i vs np s t =
    tree (treeAdd a i vs np s t) . taintData np s t . taintTypeDependants np . taintImplicitAdditions np


vsSet ::
    (MonadFail m, HasUvtt v TaintTracker) =>
    Maybe Attributee -> Interpolation -> [WireValue] ->
    NodePath -> Maybe Site -> Time -> Valuespace v -> m (Valuespace v)
vsSet a i vs np s t =
    tree (treeSet a i vs np s t) . taintData np s t . taintTypeDependants np . taintImplicitAdditions np

vsRemove ::
    (MonadFail m, HasUvtt v TaintTracker) =>
    Maybe Attributee -> NodePath -> Maybe Site -> Time ->
    Valuespace v -> m (Valuespace v)
vsRemove a np s t =
    tree (treeRemove a np s t) . taintData np s t

vsClear ::
    (MonadFail m, HasUvtt v TaintTracker) =>
    Maybe Attributee -> NodePath -> Maybe Site -> Time ->
    Valuespace v -> m (Valuespace v)
vsClear a np s t =
    tree (treeClear a np s t) . taintData np s t

vsSetChildren ::
    (MonadFail m, HasUvtt v TaintTracker) =>
    NodePath -> [Path.Seg] -> Valuespace v -> m (Valuespace v)
vsSetChildren np cns =
    tree (treeSetChildren np cns) .
    set (unvalidated . uvtt . at np) (Just Nothing) .
    taintImplicitAdditions np

globalSite = Nothing
anon = Nothing
tconst = Time 0 0

addConst ::
    (MonadFail m) => NodePath -> [WireValue] -> Valuespace OwnerUnvalidated ->
    m (Valuespace OwnerUnvalidated)
addConst np cvs = vsAdd anon IConstant cvs np globalSite tconst

define ::
    (OfMetaType def, MonadFail m) => NodePath -> def
    -> Valuespace OwnerUnvalidated -> m (Valuespace OwnerUnvalidated)
define defPath def vs =
    addConst defPath (toWireValues def) vs >>=
    return . vsAssignType defPath (metaTypePath . metaType $ def)


autoDefineStruct ::
    (MonadFail m) => NodePath -> TypePath -> Valuespace OwnerUnvalidated ->
    m (Valuespace OwnerUnvalidated)
autoDefineStruct np tp vs =
  let node = view (tree . at np . non mempty) vs in
  do
    def <- wellDefinedStruct node >>= uncurry autoGenStructDef
    define tp def vs >>= return . vsAssignType np tp
  where
    wellDefinedStruct n = sequence (
      view getKeys n,
      traverse (getType vs) $ getChildPaths np n)

autoGenStructDef
  :: (MonadFail m) => [Path.Seg] -> [TypePath] -> m StructDefinition
autoGenStructDef childNames childTypes =
    StructDefinition "auto-generated container" <$>
    (alFromZip childNames $ zip childTypes $ repeat Cannot)


assert :: (a -> Bool) -> (a -> String) -> a -> a
assert p s a | p a = a
             | otherwise = error $ printf "assertion failed %s" (s a)


baseValuespace :: Valuespace Validated
baseValuespace =
    either error (snd . assert (null . fst) (show . fst) . vsValidate) $
    return (ownerUnlock mempty) >>=
    define (metaTypePath Tuple) baseTupleDef >>=
    define (metaTypePath Struct) baseStructDef >>=
    define (metaTypePath Array) baseArrayDef >>=
    define versionDefPath versionDef >>=
    return . vsAssignType versionPath versionDefPath >>=
    addConst versionPath [WireValue @Word32 0, WireValue @Int32 (-1023)] >>=

    autoDefineStruct
        [pathq|/api/types/base|]
        [pathq|/api/types/containers/base|] >>=
    return . vsAssignType
        [pathq|/api/types/containers|]
        [pathq|/api/types/containers/containers|] >>=
    autoDefineStruct
        [pathq|/api/types/self|]
        [pathq|/api/types/containers/types_self|] >>=
    autoDefineStruct
        [pathq|/api/self|]
        [pathq|/api/types/containers/self|] >>=
    autoDefineStruct
        [pathq|/api/types|]
        [pathq|/api/types/containers/types|] >>=
    autoDefineStruct
        [pathq|/api|]
        [pathq|/api/types/containers/api|] >>=
    autoDefineStruct
        Path.Root
        [pathq|/api/types/containers/root|] >>=
    return . vsAssignType
        [pathq|/api/types/containers/containers|]
        (metaTypePath Struct) >>=
    autoDefineStruct
        [pathq|/api/types/containers|]
        [pathq|/api/types/containers/containers|]
  where
    versionPath = [pathq|/api/self/version|]
    versionDefPath = [pathq|/api/types/self/version|]
    versionDef = TupleDefinition
        "API Version"
        (fromJust $ mkAssocList
          [ ([segq|maj|], TtConc $ TcWord32 unbounded)
          , ([segq|min|], TtConc $ TcInt32 unbounded)])
        mempty

{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Clapi.Valuespace where

import Prelude hiding (fail)
import Control.Monad (liftM, when, (>=>))
import Control.Monad.Fail (MonadFail, fail)
import Control.Monad.State (State, modify)
import Control.Lens ((&), makeLenses, view, at, over, set, non, _Just, _1, _2)
import Data.Either (isLeft)
import Data.Maybe (isJust, fromJust)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map.Strict.Merge (
    merge, zipWithMaybeMatched, dropMissing, preserveMissing)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Text.Printf (printf)

import Data.Attoparsec.Text (parseOnly)

import qualified Data.Map.Mos as Mos

import Data.Maybe.Clapi (note)
import qualified Data.Maybe.Clapi as Maybe

import Clapi.Util (duplicates, eitherFail, partitionDifferenceL)
import Clapi.Path ((+|))
import qualified Clapi.Path as Path
import qualified Path.Parsing as Path
import Clapi.Types (
    CanFail, ClapiValue(..), InterpolationType(..), Interpolation(..), Time(..),
    Enumerated(..), toClapiValue, fromClapiValue, getEnum, interpolationType)
import Clapi.Tree (
    ClapiTree, NodePath, TypePath, Site, Attributed, Attributee,
    TimePoint, SiteMap, treeInitNode, treeDeleteNode, treeAdd, treeSet,
    treeRemove, treeClear, getKeys, getSites, unwrapTimePoint, getChildPaths,
    TreeDelta, treeDiff)
import qualified Clapi.Tree as Tree
import Clapi.Validator (Validator, fromText, enumDesc, validate, desc)
import Clapi.PathQ

type Node = Tree.Node [ClapiValue]

fromLeft :: Either a b -> a
fromLeft (Left a) = a

fromRight :: Either a b -> b
fromRight (Right b) = b

mapFilterJust :: Map.Map k (Maybe a) -> Map.Map k a
mapFilterJust = fmap fromJust . Map.filter isJust

data Liberty = Cannot | May | Must deriving (Show, Eq, Ord, Enum, Bounded)

data MetaType = Tuple | Struct | Array deriving (Eq, Show)
data Definition =
    TupleDef {
      _doc :: T.Text,
      _valueNames :: [Path.Name],
      _validators :: [Validator],
      _permittedInterpolations :: Set.Set InterpolationType}
  | StructDef {
      _doc :: T.Text,
      _childNames :: [Path.Name],
      _childTypes :: [Path.Path],
      _childLiberties :: [Liberty]}
  | ArrayDef {
      _doc :: T.Text,
      _childType :: Path.Path,
      _childLiberty :: Liberty}
  deriving (Show, Eq)
makeLenses ''Definition

-- Grr, boilerplate because of validators not being showable, equatable:
-- instance Show Definition where
--     show (TupleDef l _ vns vds _ is) =
--         printf "<TupleDef %s %s %s %s>" (show l) (show vns) (show vds) (show is)
--     show (StructDef l _ ns ts ls) =
--         printf "<StructDef %s %s %s %s>" (show l) (show ns) (show ts) (show ls)
--     show (ArrayDef l _ ct cl) =
--         printf "<ArrayDef %s %s %s>" (show l) (show ct) (show cl)

tupleDef ::
    (MonadFail m) => T.Text -> [Path.Name] -> [T.Text] ->
    Set.Set InterpolationType -> m Definition
tupleDef doc valueNames validatorDescs permittedInterpolations =
   let
     lvn = length valueNames
     lvd = length validatorDescs
   in do
     when (not . null . duplicates $ valueNames) (
        fail $ printf "duplicate valueNames")
     when (lvn /= lvd) (
        fail $ printf
        "mistmatched number of valueNames (%v) and validator descriptions (%v)"
        lvn lvd)
     validators <- mapM (eitherFail . fromText) validatorDescs
     return $
        TupleDef doc valueNames validators
        permittedInterpolations

structDef ::
    (MonadFail m) => T.Text -> [Path.Name] -> [TypePath] ->
    [Liberty] -> m Definition
structDef doc childNames childTypes childLiberties =
  let
    lcn = length childNames
    lct = length childTypes
    lcl = length childLiberties
  in do
    when (lcn /= lct || lcn /= lcl) (
        fail $ printf
        "mismatched number of child names (%v), types (%v) and liberties(%v)"
        lcn lct lcl)
    return $ StructDef doc childNames childTypes childLiberties

arrayDef ::
    (MonadFail m) => T.Text -> TypePath -> Liberty -> m Definition
arrayDef d ct cl = return $ ArrayDef d ct cl

apiRoot :: Path.Path
apiRoot = [pathq|/api|]

metaType :: Definition -> MetaType
metaType (TupleDef {}) = Tuple
metaType (StructDef {}) = Struct
metaType (ArrayDef {}) = Array

metaTypePath :: MetaType -> Path.Path
metaTypePath Tuple = [pathq|/api/types/base/tuple|]
metaTypePath Struct = [pathq|/api/types/base/struct|]
metaTypePath Array = [pathq|/api/types/base/array|]

categoriseMetaTypePath :: (MonadFail m) => TypePath -> m MetaType
categoriseMetaTypePath mtp
    | mtp == metaTypePath Tuple = return Tuple
    | mtp == metaTypePath Struct = return Struct
    | mtp == metaTypePath Array = return Array
    | otherwise = fail "Weird metapath!"

libertyDesc = enumDesc Cannot
interpolationTypeDesc = enumDesc ITConstant
listDesc d = T.pack $ printf "list[%v]" d
setDesc d = T.pack $ printf "set[%v]" d
-- FIXME: would like to include and share a regex for names:
namesDesc = "set[string[]]"
typeDesc = "ref[/api/types/base]"

baseTupleDef = fromJust $ tupleDef
    "t" ["doc", "valueNames", "validators", "interpolationTypes"]
    ["string", namesDesc, "list[validator]", setDesc interpolationTypeDesc] mempty
baseStructDef = fromJust $ tupleDef
    "s" ["doc", "childNames", "childTypes", "clibs"]
    ["string", namesDesc, listDesc typeDesc, listDesc libertyDesc] mempty
baseArrayDef = fromJust $ tupleDef
    "a" ["doc", "childType", "clib"]
    ["string", typeDesc, libertyDesc] mempty

defToValues :: Definition -> [ClapiValue]
defToValues (TupleDef d ns vs is) =
  [
    toClapiValue d,
    toClapiValue $ ns,
    toClapiValue $ desc <$> vs,
    toClapiValue $ Enumerated <$> Set.toList is
  ]
defToValues (StructDef d ns ts ls) =
  [
    toClapiValue d,
    toClapiValue $ ns,
    toClapiValue $ T.pack . Path.toString <$> ts,
    toClapiValue $ Enumerated <$> ls
  ]
defToValues (ArrayDef d t cl) =
  [
    toClapiValue d,
    toClapiValue $ T.pack $ Path.toString t,
    toClapiValue $ Enumerated cl
  ]


valuesToDef :: (MonadFail m) => MetaType -> [ClapiValue] -> m Definition
valuesToDef
    Tuple [ClString d, ns@(ClList _), vds@(ClList _), is@(ClList _)] =
  do
    -- FIXME: need to be able to unpack enums
    ns' <- fromClapiValue ns
    vds' <- fromClapiValue vds
    is' <- Set.fromList <$> fmap getEnum <$> fromClapiValue is
    tupleDef d ns' vds' is'
valuesToDef
    Struct [ClString d, ns@(ClList _), ts@(ClList _), ls@(ClList _)] =
  do
    ns' <- fromClapiValue ns
    ts' <- fmap T.unpack <$> fromClapiValue ts
    ts'' <- mapM Path.fromString ts'
    ls' <- fmap getEnum <$> fromClapiValue ls
    structDef d ns' ts'' ls'
valuesToDef
    Array [ClString d, ClString t, cl@(ClEnum _)] =
  do
    t' <- Path.fromString $ T.unpack t
    cl' <- getEnum <$> fromClapiValue cl
    arrayDef d t' cl'
-- TODO: This error doesn't give you much of a hint
valuesToDef mt _ = fail $ printf "bad types to define %s" (show mt)


type VsTree = ClapiTree [ClapiValue]
type DefMap = Map.Map NodePath Definition
data Validated
data Unvalidated
data Valuespace v = Valuespace {
    _tree :: VsTree,
    _types :: Mos.Dependencies NodePath TypePath,
    _xrefs :: Mos.Dependencies' (NodePath, (Maybe Site, Time)) NodePath,
    _defs :: DefMap,
    _unvalidated :: Map.Map NodePath (Maybe (Set.Set (Maybe Site, Time)))
    } deriving (Eq)
makeLenses ''Valuespace

unvalidate :: Valuespace v -> Valuespace Unvalidated
unvalidate (Valuespace tr ty x d u) = Valuespace tr ty x d u

vsGetTree :: Valuespace v -> VsTree
vsGetTree = view tree

type VsDelta = (NodePath, Either TypePath (TreeDelta [ClapiValue]))

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

instance Monoid (Valuespace v) where
    mempty = Valuespace mempty mempty mempty mempty mempty
    mappend (Valuespace a1 b1 c1 d1 e1) (Valuespace a2 b2 c2 d2 e2) =
        Valuespace (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2)

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

getChildren :: (MonadFail m) => NodePath -> Valuespace v -> m [Path.Name]
getChildren np vs = getNode np vs >>= return . view getKeys

type MonadErrorMap a = (Map.Map NodePath String, a)

eitherErrorMap ::
    Map.Map NodePath (Either String a) -> MonadErrorMap (Map.Map NodePath a)
eitherErrorMap =
    over _1 (fmap fromLeft) .
    over _2 (fmap fromRight) .
    Map.partition isLeft

vsValidate :: Valuespace Unvalidated -> MonadErrorMap (Valuespace Validated)
vsValidate =
    rectifyTypes >=> validateChildren >=> validateData >=> markValid
  where
    markValid = return . set unvalidated mempty

rectifyTypes ::
    Valuespace Unvalidated -> MonadErrorMap (Valuespace Unvalidated)
rectifyTypes vs =
  do
    unvalidatedsTypes <-
        eitherErrorMap . Map.mapWithKey (\np _ -> getType vs np) .
        view unvalidated $ vs
    newDefs <-
        eitherErrorMap . Map.mapWithKey toDef $ toMetaTypes unvalidatedsTypes
    return . over defs (Map.union newDefs) $ vs
  where
    toMetaTypes = mapFilterJust . fmap categoriseMetaTypePath
    toDef np mt = getNode np vs >>= validateTypeNode mt

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

validateChildren ::
    Valuespace Unvalidated -> MonadErrorMap (Valuespace Unvalidated)
validateChildren vs =
  let
    unvalidatedNodes = Map.restrictKeys (view tree vs) (Map.keysSet $
        view unvalidated vs)
  in do
    nodesWithDefs <- eitherErrorMap $ Map.mapWithKey pairDef unvalidatedNodes
    eitherErrorMap $ Map.mapWithKey
        (\np (n, d) -> validateNodeChildren (getType vs) np n d) nodesWithDefs
    return vs
  where
    pairDef np n = (,) n <$> getDef np vs


validateChildKeyTypes ::
    (MonadFail m) => (NodePath -> m TypePath) -> NodePath ->
    [Path.Name] -> [TypePath] -> m ()
validateChildKeyTypes getType' np orderedKeys expectedTypes =
  let
    expectedTypeMap = Map.fromList $
        zip orderedKeys (zip expectedTypes ((np +|) <$> orderedKeys))
    failTypes bad = when (not . null $ bad) $ fail $
        printf "bad child types for %s:%s" (show np) (concatMap fmtBct $ Map.assocs bad)
    fmtBct :: (Path.Name, (Path.Path, Path.Path)) -> String
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
validateNodeChildren getType' np node def@(StructDef {}) =
  let
    expectedKeys = view childNames def
    expectedTypes = view childTypes def
    nodeKeys = view getKeys node
    (missing, extra) = partitionDifferenceL expectedKeys nodeKeys
    failKeys = fail $
        printf "expected node keys %s, got %s" (show expectedKeys)
        (show nodeKeys)
  in do
    when ((missing, extra) /= mempty) failKeys
    validateChildKeyTypes getType' np expectedKeys expectedTypes
validateNodeChildren getType' np node (ArrayDef _doc expectedType childLiberty) =
  let
    nodeKeys = view getKeys node
    dups = duplicates nodeKeys
    failDups = when (not . null $ dups) $ fail $
        printf "duplicate array keys %s" (show dups)
    failTypes bad = when (not . null $ bad) $ fail $
        printf "bad child types %s" (show bad)
  in do
    mapM_ (eitherFail . parseOnly Path.nameP) nodeKeys
    failDups
    validateChildKeyTypes getType' np nodeKeys (repeat expectedType)


flattenNestedMaps ::
    (Ord k1, Ord k2) => Map.Map k1 (Map.Map k2 a) -> Map.Map (k1, k2) a
flattenNestedMaps mm = foldMap id $ Map.mapWithKey f mm
  where
    f k1 = Map.mapKeys ((,) k1)

validateData ::
    Valuespace Unvalidated -> MonadErrorMap (Valuespace Unvalidated)
validateData vs =
  do
    newXRefDeps <- overUnvalidatedNodes (validateNodeData vs) vs
    return $ over xrefs (f newXRefDeps) vs
  where
    f newXRefDeps unv =
        Map.foldrWithKey Mos.setDependencies' unv
        (flattenNestedMaps newXRefDeps)

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
    Map.Map (Maybe Site, Time) (Interpolation, [ClapiValue])
filterNode mtps n =
  let
    sites = view getSites n
    tps = maybe allTps id mtps
    allTps = Map.keysSet . flattenNestedMaps $ sites
  in
    filterSiteMap sites tps

overUnvalidatedNodes ::
    (
       NodePath -> Map.Map (Maybe Site, Time) (Interpolation, [ClapiValue]) ->
       CanFail a
    ) -> Valuespace v -> MonadErrorMap (Map.Map NodePath a)
overUnvalidatedNodes f vs =
  do
    filtered <-
        eitherErrorMap . Map.mapWithKey getAndFilterNode $ view unvalidated vs
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
    Map.Map (Maybe Site, Time) (Interpolation, [ClapiValue]) ->
    m (Map.Map (Maybe Site, Time) [NodePath])
validateNodeData vs np siteMap = getDef np vs >>= body
  where
    body def@(TupleDef {}) = do
        let its = view permittedInterpolations def
        mapM_ (validateInterpolation its . fst) siteMap
        let vals = view validators def
        mapM (eitherFail . validate (getType vs) vals . snd) siteMap
    body _ | siteMap == mempty = return mempty
           | otherwise = fail "data found in container"

-- vsUpdateXRefs :: Valuespace v -> MonadErrorMap (Valuespace Validated)
-- vsUpdateXRefs vs =
--     eitherErrorMap . Map.mapWithKey getXRefDeps $ unvalidatedNodes vs
--   where
--     getXRefDeps np n = do
--         def <- getDef np vs
--         traverse (validate (getType vs) (view validators def)) (toList n)

vsAssignType :: NodePath -> TypePath -> Valuespace v -> Valuespace Unvalidated
vsAssignType np tp =
    over tree (treeInitNode np) .
    over types (Mos.setDependency np tp) .
    set (unvalidated . at np) (Just Nothing) .
    set (unvalidated . at (Path.up np)) (Just Nothing) .
    taintXRefDependants np

vsDelete ::
    (MonadFail m) => NodePath -> Valuespace v -> m (Valuespace Unvalidated)
vsDelete np =
    tree (treeDeleteNode np) .
    over types (Mos.delDependency np) .
    set (unvalidated . at np) Nothing .
    set (unvalidated . at (Path.up np)) (Just Nothing)

taintData ::
    NodePath -> Maybe Site -> Time -> Valuespace v -> Valuespace Unvalidated
taintData np s t =
    over (unvalidated . at np . non mempty . non mempty) (Set.insert (s, t))

taintTypeDependants :: NodePath -> Valuespace v -> Valuespace Unvalidated
taintTypeDependants np vs =
    over unvalidated (Maybe.update Map.union maybeTaintedMap) vs
  where
    maybeTaintedMap = Map.fromSet (const Nothing) <$>
        (Mos.getDependants np $ view types vs)

taintXRefDependants :: NodePath -> Valuespace v -> Valuespace Unvalidated
taintXRefDependants np vs = over unvalidated (Map.union taintedMap) vs
  where
    taintedMap =
        fmap Just . Set.foldr (uncurry Mos.insert) mempty .
        Mos.getDependants' np $ view xrefs vs

vsAdd ::
    (MonadFail m) => Maybe Attributee -> Interpolation -> [ClapiValue] ->
    NodePath -> Maybe Site -> Time -> Valuespace v -> m (Valuespace Unvalidated)
vsAdd a i vs np s t =
    tree (treeAdd a i vs np s t) . taintData np s t . taintTypeDependants np


vsSet ::
    (MonadFail m) => Maybe Attributee -> Interpolation -> [ClapiValue] ->
    NodePath -> Maybe Site -> Time -> Valuespace v -> m (Valuespace Unvalidated)
vsSet a i vs np s t =
    tree (treeSet a i vs np s t) . taintData np s t . taintTypeDependants np

vsRemove ::
    (MonadFail m) => Maybe Attributee -> NodePath -> Maybe Site -> Time ->
    Valuespace v -> m (Valuespace Unvalidated)
vsRemove a np s t =
    tree (treeRemove a np s t) . taintData np s t

vsClear ::
    (MonadFail m) => Maybe Attributee -> NodePath -> Maybe Site -> Time ->
    Valuespace v -> m (Valuespace Unvalidated)
vsClear a np s t =
    tree (treeClear a np s t) . taintData np s t


globalSite = Nothing
anon = Nothing
tconst = Time 0 0

addConst ::
    (MonadFail m) => NodePath -> [ClapiValue] -> Valuespace v ->
    m (Valuespace Unvalidated)
addConst np cvs = vsAdd anon IConstant cvs np globalSite tconst

define ::
    (MonadFail m) => NodePath -> Definition -> Valuespace v ->
    m (Valuespace Unvalidated)
define defPath def vs =
    addConst defPath (defToValues def) vs >>=
    return . vsAssignType defPath (metaTypePath . metaType $ def)


autoDefineStruct ::
    (MonadFail m) => NodePath -> TypePath -> Valuespace v ->
    m (Valuespace Unvalidated)
autoDefineStruct np tp vs =
  let node = view (tree . at np . non mempty) vs in
  do
    def <- wellDefinedStruct node >>= autoGenStructDef
    define tp def vs >>= return . vsAssignType np tp
  where
    wellDefinedStruct n = sequence (
      view getKeys n,
      traverse (getType vs) $ getChildPaths np n)

autoGenStructDef :: (MonadFail m) => ([Path.Name], [TypePath]) -> m Definition
autoGenStructDef (childNames, childTypes) =
    structDef "auto-generated container" childNames childTypes
    (fmap (const Cannot) childNames)


assert :: (a -> Bool) -> (a -> String) -> a -> a
assert p s a | p a = a
             | otherwise = error $ printf "assertion failed %s" (s a)


baseValuespace :: Valuespace Validated
baseValuespace =
    either error (snd . assert (null . fst) (show . fst) . vsValidate) $
    return mempty >>=
    define (metaTypePath Tuple) baseTupleDef >>=
    define (metaTypePath Struct) baseStructDef >>=
    define (metaTypePath Array) baseArrayDef >>=
    define versionDefPath versionDef >>=
    define buildDefPath buildDef >>=
    return . vsAssignType versionPath versionDefPath >>=
    addConst versionPath [ClWord32 0, ClInt32 (-1023)] >>=
    return . vsAssignType buildPath buildDefPath >>=
    addConst buildPath [ClString "banana"] >>=

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
        Path.root
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
    versionDef = fromJust $ tupleDef
        "t" ["maj", "min"] ["word32", "int32"] mempty

    -- See if making path an isString that errors gives us friendly compile time failures
    buildPath = [pathq|/api/self/build|]
    buildDefPath = [pathq|/api/types/self/build|]
    buildDef = fromJust $ tupleDef
        "b" ["commit_hash"] ["string[banana]"] mempty

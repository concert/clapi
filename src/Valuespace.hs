{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Valuespace where

import Prelude hiding (fail)
import Control.Monad (liftM, when, (>=>))
import Control.Monad.Fail (MonadFail, fail)
import Control.Monad.State (State, modify)
import Control.Lens ((&), makeLenses, view, at, over, set, non, _Just, _1, _2)
import Data.Either (isLeft)
import Data.Maybe (isJust, fromJust)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Text as T
import Text.Printf (printf)

import Data.Attoparsec.Text (parseOnly)

import qualified Data.Map.Mos as Mos

import Data.Maybe.Clapi (note)

import Util (duplicates, eitherFail, (+|), partitionDifferenceL)
import qualified Path
import qualified Path.Parsing as Path
import Types (
    CanFail, ClapiValue(..), InterpolationType(..), Interpolation(..), Time(..),
    Enumerated(..), toClapiValue, fromClapiValue, getEnum, interpolationType)
import Tree (
    ClapiTree, NodePath, TypePath, Site, Attributed, Attributee,
    TimePoint, SiteMap, treeInitNode, treeDeleteNode, treeAdd, treeSet,
    treeRemove, treeClear, getKeys, getSites, unwrapTimePoint, getChildPaths)
import qualified Tree as Tree
import Validator (Validator, fromText, enumDesc, validate)

type Node = Tree.Node [ClapiValue]

fromLeft :: Either a b -> a
fromLeft (Left a) = a

fromRight :: Either a b -> b
fromRight (Right b) = b

mapFilterJust :: Map.Map k (Maybe a) -> Map.Map k a
mapFilterJust = fmap fromJust . Map.filter isJust

data Liberty = Cannot | May | Must deriving (Show, Eq, Enum, Bounded)

data MetaType = Tuple | Struct | Array deriving (Eq, Show)
data Definition =
    TupleDef {
      _liberty :: Liberty,
      _doc :: T.Text,
      _valueNames :: [Path.Name],
      _validatorDescs :: [T.Text],
      _validators :: [Validator],
      _permittedInterpolations :: Set.Set InterpolationType}
  | StructDef {
      _liberty :: Liberty,
      _doc :: T.Text,
      _childNames :: [Path.Name],
      _childTypes :: [Path.Path],
      _childLiberties :: [Liberty]}
  | ArrayDef {
      _liberty :: Liberty,
      _doc :: T.Text,
      _childType :: Path.Path,
      _childLiberty :: Liberty}
makeLenses ''Definition

-- Grr, boilerplate because of validators not being showable, equatable:
instance Show Definition where
    show (TupleDef l _ vns vds _ is) =
        printf "<TupleDef %s %s %s $s>" (show l) (show vns) (show vds) (show is)
    show (StructDef l _ ns ts ls) =
        printf "<StructDef %s %s %s %s>" (show l) (show ns) (show ts) (show ls)
    show (ArrayDef l _ ct cl) =
        printf "<ArrayDef %s %s %s>" (show l) (show ct) (show cl)

instance Eq Definition where
    (TupleDef l1 d1 ns1 vds1 _ is1) == (TupleDef l2 d2 ns2 vds2 _ is2) =
        l1 == l2 && d1 == d2 && ns1 == ns2 && vds1 == vds2 && is1 == is2
    (StructDef l1 d1 ns1 ts1 ls1) == (StructDef l2 d2 ns2 ts2 ls2) =
        l1 == l2 && d1 == d2 && ns1 == ns2 && ts1 == ts2 && ls1 == ls2
    (ArrayDef l1 d1 t1 cl1) == (ArrayDef l2 d2 t2 cl2) =
        l1 == l2 && d1 == d2 && t1 == t2 && cl1 == cl2

tupleDef ::
    (MonadFail m) => Liberty -> T.Text -> [Path.Name] -> [T.Text] ->
    Set.Set InterpolationType -> m Definition
tupleDef liberty doc valueNames validatorDescs permittedInterpolations =
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
        TupleDef liberty doc valueNames validatorDescs validators
        permittedInterpolations

structDef ::
    (MonadFail m) => Liberty -> T.Text -> [Path.Name] -> [TypePath] ->
    [Liberty] -> m Definition
structDef liberty doc childNames childTypes childLiberties =
  let
    lcn = length childNames
    lct = length childTypes
    lcl = length childLiberties
  in do
    when (lcn /= lct || lcn /= lcl) (
        fail $ printf
        "mismatched number of child names (%v), types (%v) and liberties(%v)"
        lcn lct lcl)
    return $ StructDef liberty doc childNames childTypes childLiberties

arrayDef ::
    (MonadFail m) => Liberty -> T.Text -> TypePath -> Liberty -> m Definition
arrayDef l d ct cl = return $ ArrayDef l d ct cl

apiRoot :: Path.Path
apiRoot = ["api"]

metaType :: Definition -> MetaType
metaType (TupleDef {}) = Tuple
metaType (StructDef {}) = Struct
metaType (ArrayDef {}) = Array

metaTypePath :: MetaType -> Path.Path
metaTypePath Tuple = ["api", "types", "base", "tuple"]
metaTypePath Struct = ["api", "types", "base", "struct"]
metaTypePath Array = ["api", "types", "base", "array"]

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
    Cannot "t" ["liberty", "doc", "valueNames", "validators", "interpolationTypes"]
    [libertyDesc, "string", namesDesc, "list[validator]", setDesc interpolationTypeDesc] mempty
baseStructDef = fromJust $ tupleDef
    Cannot "s" ["liberty", "doc", "childNames", "childTypes", "clibs"]
    [libertyDesc, "string", namesDesc, listDesc typeDesc, listDesc libertyDesc] mempty
baseArrayDef = fromJust $ tupleDef
    Cannot "a" ["liberty", "doc", "childType", "clib"]
    [libertyDesc, "string", typeDesc, libertyDesc] mempty

defToValues :: Definition -> [ClapiValue]
defToValues (TupleDef l d ns vds vs is) =
  [
    toClapiValue $ Enumerated l,
    toClapiValue d,
    toClapiValue $ T.pack <$> ns,
    toClapiValue vds,
    toClapiValue $ Enumerated <$> Set.toList is
  ]
defToValues (StructDef l d ns ts ls) =
  [
    toClapiValue $ Enumerated l,
    toClapiValue d,
    toClapiValue $ T.pack <$> ns,
    toClapiValue $ T.pack . Path.toString <$> ts,
    toClapiValue $ Enumerated <$> ls
  ]
defToValues (ArrayDef l d t cl) =
  [
    toClapiValue $ Enumerated l,
    toClapiValue d,
    toClapiValue $ T.pack $ Path.toString t,
    toClapiValue $ Enumerated cl
  ]


valuesToDef :: (MonadFail m) => MetaType -> [ClapiValue] -> m Definition
valuesToDef
    Tuple [l@(CEnum _), CString d, ns@(CList _), vds@(CList _), is@(CList _)] =
  do
    -- FIXME: need to be able to unpack enums
    l' <- getEnum <$> fromClapiValue l
    ns' <- fmap T.unpack <$> fromClapiValue ns
    vds' <- fromClapiValue vds
    is' <- Set.fromList <$> fmap getEnum <$> fromClapiValue is
    tupleDef l' d ns' vds' is'
valuesToDef
    Struct [l@(CEnum _), CString d, ns@(CList _), ts@(CList _), ls@(CList _)] =
  do
    l' <- getEnum <$> fromClapiValue l
    ns' <- fmap T.unpack <$> fromClapiValue ns
    ts' <- fmap T.unpack <$> fromClapiValue ts
    ts'' <- mapM Path.fromString ts'
    ls' <- fmap getEnum <$> fromClapiValue ls
    structDef l' d ns' ts'' ls'
valuesToDef
    Array [l@(CEnum _), CString d, CString t, cl@(CEnum _)] =
  do
    l' <- getEnum <$> fromClapiValue l
    t' <- Path.fromString $ T.unpack t
    cl' <- getEnum <$> fromClapiValue cl
    arrayDef l' d t' cl'
valuesToDef mt _ = fail $ printf "bad types to define %s" (show mt)


type VsTree = ClapiTree [ClapiValue]
type DefMap = Map.Map NodePath Definition
data Validated
data Unvalidated
data Valuespace v = Valuespace {
    _tree :: VsTree,
    _types :: Mos.Dependencies NodePath TypePath,
    _xrefs :: Mos.Dependencies NodePath NodePath,
    _defs :: DefMap,
    _unvalidated :: Map.Map NodePath (Maybe (Set.Set (Maybe Site, Time)))
    } deriving (Eq)
makeLenses ''Valuespace

instance Show (Valuespace v) where
    show = show . view tree

instance Monoid (Valuespace v) where
    mempty = Valuespace mempty mempty mempty mempty mempty
    mappend (Valuespace a1 b1 c1 d1 e1) (Valuespace a2 b2 c2 d2 e2) =
        Valuespace (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2)

getType :: (MonadFail m) => NodePath -> Valuespace v -> m TypePath
getType np = note f . Mos.getDependency np . view types
  where
    f = printf "No type path specified for %s" (show np)

getDef :: (MonadFail m) => NodePath -> Valuespace v -> m Definition
getDef np vs =
  do
    tp <- getType np vs
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
        eitherErrorMap . Map.mapWithKey (\np _ -> getType np vs) .
        view unvalidated $ vs
    newDefs <-
        eitherErrorMap . Map.mapWithKey toDef $ toMetaTypes unvalidatedsTypes
    return .
        over defs (Map.union newDefs) .
        over unvalidated (flip Map.difference newDefs) $
        vs
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
  let nodes = Map.restrictKeys (view tree vs) (Map.keysSet $ view unvalidated vs) in
  do
    nodesWithDefs <- eitherErrorMap $ Map.mapWithKey pairDef nodes
    eitherErrorMap $ fmap (uncurry validateNodeChildren) nodesWithDefs
    return vs
  where
    pairDef np n = (,) n <$> getDef np vs


validateNodeChildren :: (MonadFail m) => Node -> Definition -> m ()
validateNodeChildren node (TupleDef {}) = case view getKeys node of
  [] -> return ()
  _ -> fail "tuple node has children"
validateNodeChildren node def@(StructDef {}) =
  let
    expectedKeys = view childNames def
    nodeKeys = view getKeys node
    (missing, extra) = partitionDifferenceL expectedKeys nodeKeys
  in
  if (missing, extra) == mempty
    then return ()
    else fail $
      printf "expected node keys %s, got %s" (show expectedKeys) (show nodeKeys)
validateNodeChildren node (ArrayDef {}) =
    mapM_ (eitherFail . parseOnly Path.nameP . T.pack) (view getKeys node)


flattenNestedMaps ::
    (Ord k1, Ord k2) => Map.Map k1 (Map.Map k2 a) -> Map.Map (k1, k2) a
flattenNestedMaps mm = foldMap id $ Map.mapWithKey f mm
  where
    f k1 = Map.mapKeys ((,) k1)

validateData ::
    Valuespace Unvalidated -> MonadErrorMap (Valuespace Unvalidated)
validateData vs =
  do
    (eitherErrorMap . Map.mapWithKey (vsValidatePath vs) $ view unvalidated vs)
    return vs


vsValidatePath ::
    (MonadFail m) => Valuespace v -> NodePath ->
    Maybe (Set.Set (Maybe Site, Time)) -> m ()
vsValidatePath vs np mtps =
  do
    def <- getDef np vs
    node <- getNode np vs
    case def of
      (TupleDef {}) -> case mtps of
        (Just tps) -> validateNodeData getType' def node tps
        Nothing -> validateNodeData getType' def node $ allTps node
      _ -> if view getSites node == mempty
        then return ()
        else fail $ printf "data found in container node at %s" (show np)
  where
    getType' = flip getType vs
    allTps node = Map.keysSet . flattenNestedMaps $ view getSites node


validateInterpolation ::
    (MonadFail m) => Set.Set InterpolationType -> Interpolation -> m ()
validateInterpolation its i | it `elem` its = return ()
                           | otherwise = fail f
  where
    it = interpolationType i
    f =  printf "forbidden interpolation type %s (allowed %s)"
        (show it) (show its)


validateNodeData ::
    (MonadFail m) => (NodePath -> CanFail TypePath) -> Definition -> Node ->
    Set.Set (Maybe Site, Time) -> m ()
validateNodeData getType' def node tps =
  let
    vals = view validators def
    its = view permittedInterpolations def
    siteMap = filterSiteMap (view getSites node) tps
  in do
    mapM_ (eitherFail . validate getType' vals . snd) siteMap
    mapM_ (validateInterpolation its . fst) siteMap
  where
    -- This doesn't need to repack the values back into a full SiteMap :-)
    filterSiteMap ::
        SiteMap a -> Set.Set (Maybe Site, Time) ->
        Map.Map (Maybe Site, Time) (Interpolation, a)
    filterSiteMap sm keys = mapFilterJust $
        unwrapTimePoint <$> Map.restrictKeys (flattenNestedMaps sm) keys

vsAssignType :: NodePath -> TypePath -> Valuespace v -> Valuespace Unvalidated
vsAssignType np tp =
    over tree (treeInitNode np) .
    over types (Mos.setDependency np tp) .
    set (unvalidated . at np) (Just Nothing)

vsDelete ::
    (MonadFail m) => NodePath -> Valuespace v -> m (Valuespace Unvalidated)
vsDelete np =
    tree (treeDeleteNode np) .
    over types (Mos.delDependency np) .
    set (unvalidated . at np) Nothing

vsAdd ::
    (MonadFail m) => Maybe Attributee -> Interpolation -> [ClapiValue] ->
    NodePath -> Maybe Site -> Time -> Valuespace v -> m (Valuespace Unvalidated)
vsAdd a i vs np s t =
    tree (treeAdd a i vs np s t) .
    over (unvalidated . at np . non mempty . _Just) (Set.insert (s, t))

vsSet ::
    (MonadFail m) => Maybe Attributee -> Interpolation -> [ClapiValue] ->
    NodePath -> Maybe Site -> Time -> Valuespace v -> m (Valuespace Unvalidated)
vsSet a i vs np s t =
    tree (treeSet a i vs np s t) .
    over (unvalidated . at np . non mempty . _Just) (Set.insert (s, t))

vsRemove ::
    (MonadFail m) => Maybe Attributee -> NodePath -> Maybe Site -> Time ->
    Valuespace v -> m (Valuespace Unvalidated)
vsRemove a np s t =
    tree (treeRemove a np s t) .
    over (unvalidated . at np . non mempty . _Just ) (Set.insert (s, t))

vsClear ::
    (MonadFail m) => Maybe Attributee -> NodePath -> Maybe Site -> Time ->
    Valuespace v -> m (Valuespace Unvalidated)
vsClear a np s t =
    tree (treeClear a np s t) .
    over (unvalidated . at np . non mempty . _Just) (Set.insert (s, t))


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
      traverse (flip getType vs) $ getChildPaths np n)

autoGenStructDef :: (MonadFail m) => ([Path.Name], [TypePath]) -> m Definition
autoGenStructDef (childNames, childTypes) =
    structDef Cannot "auto-generated container" childNames childTypes
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
    addConst versionPath [CWord32 0, CInt32 (-1023)] >>=
    return . vsAssignType buildPath buildDefPath >>=
    addConst buildPath [CString "banana"] >>=

    autoDefineStruct
        ["api", "types", "base"]
        ["api", "types", "containers", "base"] >>=
    return . vsAssignType
        ["api", "types", "containers"]
        ["api", "types", "containers", "containers"] >>=
    autoDefineStruct
        ["api", "types", "self"]
        ["api", "types", "containers", "types_self"] >>=
    autoDefineStruct
        ["api", "self"]
        ["api", "types", "containers", "self"] >>=
    autoDefineStruct
        ["api", "types"]
        ["api", "types", "containers", "types"] >>=
    autoDefineStruct
        ["api"]
        ["api", "types", "containers", "api"] >>=
    autoDefineStruct
        []
        ["api", "types", "containers", "root"] >>=
    return . vsAssignType
        ["api", "types", "containers", "containers"]
        (metaTypePath Tuple) >>=
    autoDefineStruct ["api", "types", "containers"]
        ["api", "types", "containers", "containers"]
  where
    versionPath = ["api", "self", "version"]
    versionDefPath = ["api", "types", "self", "version"]
    versionDef = fromJust $ tupleDef
        Cannot "t" ["maj", "min"] ["word32", "int32"] mempty

    buildPath = ["api", "self", "build"]
    buildDefPath = ["api", "types", "self", "build"]
    buildDef = fromJust $ tupleDef
        Cannot "b" ["commit_hash"] ["string[banana]"] mempty

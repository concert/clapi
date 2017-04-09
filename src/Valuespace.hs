{-# LANGUAGE OverloadedStrings #-}
module Valuespace where

import Control.Monad (liftM, when)
import Control.Monad.Fail (MonadFail)
import Control.Lens ((&))
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as T
import Text.Printf (printf)

import Util (eitherFail)
import qualified Path
import qualified Path.Parsing as Path
import Types (
    CanFail, ClapiValue(..), InterpolationType(..),
    Interpolation(..), Time(..), Enumerated(..), toClapiValue, fromClapiValue,
    getEnum)
import Tree (
  (+|), ClapiTree(..), NodePath, TypePath, treeGetType, treeInitNode,
  treeInitNodes, treeSetChildren, treeAdd)
import Validator (Validator, fromText, enumDesc)


strictZipWith :: (MonadFail m) => (a -> b -> c) -> [a] -> [b] -> m [c]
strictZipWith f [] [] = return []
strictZipWith f [] (b:bs) = fail "ran out of a's"
strictZipWith f (a:as) [] = fail "ran out of b's"
strictZipWith f (a:as) (b:bs) = (:) (f a b) <$> strictZipWith f as bs

strictZip :: (MonadFail m) => [a] -> [b] -> m [(a, b)]
strictZip = strictZipWith (,)

unpack (Right v) = v
unpack (Left v) = error v

data Liberty = Cannot | May | Must deriving (Show, Eq, Enum, Bounded)

data MetaType = Tuple | Struct | Array deriving (Eq, Show)
data Definition =
    TupleDef {
      liberty :: Liberty,
      doc :: T.Text,
      valueNames :: [Path.Name],
      validatorDescs :: [T.Text],
      validators :: [Validator],
      permittedInterpolations :: Set.Set InterpolationType}
  | StructDef {
      liberty :: Liberty,
      doc :: T.Text,
      childNames :: [Path.Name],
      childTypes :: [Path.Path],
      childLiberties :: [Liberty]}
  | ArrayDef {
      liberty :: Liberty,
      doc :: T.Text,
      childType :: Path.Path,
      childLiberty :: Liberty}

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

definitionValidators :: Definition -> [Validator]
definitionValidators t@(TupleDef {}) = validators t
definitionValidators _ = []

tupleDef ::
    (MonadFail m) => Liberty -> T.Text -> [Path.Name] -> [T.Text] ->
    Set.Set InterpolationType -> m Definition
tupleDef liberty doc valueNames validatorDescs permittedInterpolations =
   let
     lvn = length valueNames
     lvd = length validatorDescs
   in do
     when (lvn /= lvd) (
        fail $ printf
        "mistmatched number of valueNames (%v) and validator descriptions (%v)"
        lvn lvd)
     validators <- mapM (eitherFail . fromText) validatorDescs
     return $
        TupleDef liberty doc valueNames validatorDescs validators
        permittedInterpolations

structDef ::
    (MonadFail m) => Liberty -> T.Text -> [Path.Name] -> [Path.Path] ->
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
    (MonadFail m) => Liberty -> T.Text -> Path.Path -> Liberty -> m Definition
arrayDef l d ct cl = return $ ArrayDef l d ct cl

metaType :: Definition -> MetaType
metaType (TupleDef {}) = Tuple
metaType (StructDef {}) = Struct
metaType (ArrayDef {}) = Array

libertyDesc = enumDesc Cannot
interpolationTypeDesc = enumDesc ITConstant
listDesc d = T.pack $ printf "list[%v]" d
setDesc d = T.pack $ printf "set[%v]" d
-- FIXME: would like to include and share a regex for names:
namesDesc = "set[string[]]"
typeDesc = "ref[/api/types/base]"

baseTupleDef = unpack $ tupleDef
    Cannot "t" ["liberty", "doc", "valueNames", "validators", "interpolationTypes"]
    [libertyDesc, "string", namesDesc, "list[validator]", setDesc interpolationTypeDesc] mempty
baseStructDef = unpack $ tupleDef
    Cannot "s" ["liberty", "doc", "childNames", "childTypes", "clibs"]
    [libertyDesc, "string", namesDesc, listDesc typeDesc, listDesc libertyDesc] mempty
baseArrayDef = unpack $ tupleDef
    Cannot "a" ["liberty", "doc", "childType", "clib"]
    [libertyDesc, "string", typeDesc, libertyDesc] mempty

-- data Values = Tuple [ClapiValue] | Struct [ClapiValue] | Array [ClapiValue]
--   deriving (Show)

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
valuesToDef Tuple _ = fail "bad types to define tuple"
valuesToDef
    Struct [l@(CEnum _), CString d, ns@(CList _), ts@(CList _), ls@(CList _)] =
  do
    l' <- getEnum <$> fromClapiValue l
    ns' <- fmap T.unpack <$> fromClapiValue ns
    ts' <- fmap T.unpack <$> fromClapiValue ts
    ts'' <- mapM Path.fromString ts'
    ls' <- fmap getEnum <$> fromClapiValue ls
    structDef l' d ns' ts'' ls'
valuesToDef Struct _ = fail "bad types to define struct"
valuesToDef
    Array [l@(CEnum _), CString d, CString t, cl@(CEnum _)] =
  do
    l' <- getEnum <$> fromClapiValue l
    t' <- Path.fromString $ T.unpack t
    cl' <- getEnum <$> fromClapiValue cl
    arrayDef l' d t' cl'
valuesToDef Array _ = fail "bad types to define array"


type VsTree = ClapiTree [ClapiValue]
type Vmap = Map.Map NodePath [Validator]
data Valuespace = Valuespace {getTree :: VsTree, getVmap :: Vmap}

instance Show Valuespace where
    show (Valuespace t _) = show t

updateVs :: (VsTree -> CanFail VsTree) -> (Vmap -> CanFail Vmap) ->
    Valuespace -> CanFail Valuespace
updateVs f g (Valuespace vsTree vmap) =
  do
    vsTree' <- f vsTree
    vmap' <- g vmap
    return $ Valuespace vsTree' vmap'

updateVsTree :: (VsTree -> CanFail VsTree) -> Valuespace -> CanFail Valuespace
updateVsTree f = updateVs f return


apiRoot :: Path.Path
apiRoot = ["api"]

tupleTypePath :: Path.Path
tupleTypePath = ["api", "types", "base", "tuple"]

structTypePath :: Path.Path
structTypePath = ["api", "types", "base", "struct"]

arrayTypePath :: Path.Path
arrayTypePath = ["api", "types", "base", "array"]

getMetaType :: NodePath -> Valuespace -> CanFail MetaType
getMetaType p (Valuespace tree _) =
  do
    tp <- treeGetType p tree
    mtp <- treeGetType tp tree
    categorise mtp
  where
    categorise mtp
      | mtp == tupleTypePath = Right Tuple
      | mtp == structTypePath = Right Struct
      | mtp == arrayTypePath = Right Array
      | otherwise = Left "Weird metapath!"


initStruct :: NodePath -> TypePath -> Liberty -> T.Text ->
    [(Path.Name, TypePath)] -> Valuespace -> CanFail Valuespace
initStruct np tp lib doc children =
    updateVs updateTree updateVmap
  where
    updateTree tree =
        tree &
        treeInitNode tp structTypePath &
        treeInitNode np tp &
        addConst (defToValues structDef) tp >>=
        \x -> pure (treeInitNodes typePathMap x) >>=
        treeSetChildren np childNames
      where
        childNames = fmap fst children
        childTypes = fmap snd children
        prependRoot (name, tp) = (np +| name, tp)
        typePathMap = Map.fromList $ fmap prependRoot children
        structDef = StructDef lib doc childNames childTypes []
    updateVmap vmap = return undefined


globalSite = Nothing
anon = Nothing
tconst = Time 0 0

addConst :: [ClapiValue] -> NodePath -> VsTree -> CanFail VsTree
addConst cvs np = treeAdd anon IConstant cvs np globalSite tconst

getBaseValuespace :: Valuespace
getBaseValuespace = unpack (
    Valuespace mempty mempty &
    initStruct Path.root
        ["api", "types", "containers", "root"]
        Cannot "doc"
        [("api", ["api", "types", "containers", "api"])] >>=
    initStruct ["api"]
        ["api", "types", "containers", "api"]
        Cannot "doc"
        [("types", ["api", "types", "containers", "types"]),
         ("version", ["api", "types", "self", "version"]),
         ("build", ["api", "types", "self", "build"])
        ] >>=
    initStruct ["api", "types"]
        ["api", "types", "containers", "types"]
        Cannot "doc"
        [("base", ["api", "types", "containers", "base"]),
         ("self", ["api", "types", "containers", "self"]),
         ("containers", ["api", "types", "containers", "containers"])
        ] >>=
    initStruct ["api", "types", "base"]
        ["api", "types", "containers", "base"]
        Cannot "doc"
        [("tuple", tupleTypePath),
         ("struct", tupleTypePath),
         ("array", tupleTypePath)
        ] >>=
    addConst' (defToValues baseTupleDef) tupleTypePath >>=
    addConst' (defToValues baseStructDef) structTypePath >>=
    addConst' (defToValues baseArrayDef) arrayTypePath >>=
    initStruct ["api", "types", "self"]
        ["api", "types", "containers", "self"]
        Cannot "doc"
        [("version", tupleTypePath),
         ("build", tupleTypePath)
        ] >>=
    addConst' (defToValues versionDef) ["api", "types", "self", "version"] >>=
    addConst' (defToValues buildDef) ["api", "types", "self", "build"] >>=
    treeInitNode' ["api", "types", "containers"] structTypePath >>=
    treeSetChildren' ["api", "types", "containers"]
        ["root", "api", "types", "base", "self"]
    )
  where
    versionDef = unpack $ tupleDef Cannot "v" ["maj", "min"] ["word32", "int32"] mempty
    -- FIXME: want to be able to put in a regex for a sha1: [0-9a-f]{x}
    buildDef = unpack $ tupleDef Cannot "b" ["value"] ["string[banana]"] mempty
    addConst' cvs tp = updateVsTree (addConst cvs tp)
    treeInitNode' np tp = updateVsTree f
      where
        f tree = pure $ treeInitNode np tp tree
    treeSetChildren' np children = updateVsTree f
      where
        f tree = treeSetChildren np children tree

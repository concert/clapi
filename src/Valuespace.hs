module Valuespace where

import Control.Monad (liftM)
import Control.Lens ((&))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Text (pack)

import qualified Path
import qualified Path.Parsing as Path
import Types (ClapiValue(..), Interpolation(..), Time(..))
import Tree (
  CanFail, (+|), ClapiTree(..), NodePath, TypePath, treeGetType, treeInitNode,
  treeInitNodes, treeSetChildren, treeAdd)

data Liberty = Cannot | May | Must deriving (Show)
data Definition =
    TupleDef {
      liberty :: Liberty,
      doc :: String,
      valueNames :: [Path.Name],
      validators :: [String], -- [ClapiValue -> Maybe String],
      permittedInterpolations :: [Interpolation]}
  | StructDef {
      liberty :: Liberty,
      doc :: String,
      childNames :: [Path.Name],
      childTypes :: [Path.Path],
      childLiberties :: [Liberty]}
  | ArrayDef {
      liberty :: Liberty,
      doc :: String,
      childType :: Path.Path,
      childLiberty :: Liberty}
  deriving Show

baseTupleDef = TupleDef
    Cannot "t" ["liberty", "doc", "valueNames", "validators", "int"]
    ["srsly?"] mempty
baseStructDef = TupleDef
    Cannot "s" ["liberty", "doc", "childNames", "childTypes", "clibs"]
    ["srsly?"] mempty
baseArrayDef = TupleDef
    Cannot "a" ["liberty", "doc", "childType", "clib"] ["srsly?"] mempty

-- data Values = Tuple [ClapiValue] | Struct [ClapiValue] | Array [ClapiValue]
--   deriving (Show)

defToValues :: Definition -> [ClapiValue]
defToValues (TupleDef l d ns vs is) =
  [
    CString $ pack $ show l,
    CString $ pack d,
    CList $ fmap (CString . pack) ns,
    CList $ fmap (CString . pack) vs,
    CList $ fmap (CString . pack . show) is
  ]
defToValues (StructDef l d ns ts ls) =
  [
    CString $ pack $ show l,
    CString $ pack d,
    CList $ fmap (CString . pack) ns,
    CList $ fmap (CString . pack . Path.toString) ts,
    CList $ fmap (CString . pack . show) ls
  ]
defToValues (ArrayDef l d t cl) =
  [
    CString $ pack $ show l,
    CString $ pack d,
    CString $ pack $ Path.toString t,
    CString $ pack $ show cl
  ]

-- valuesToDef :: Values -> Definition
-- valuesToDef = undefined

data MetaType = Tuple | Struct | Array deriving (Eq, Show)
type Valuespace = ClapiTree [ClapiValue]

apiRoot :: Path.Path
apiRoot = ["api"]

tupleTypePath :: Path.Path
tupleTypePath = ["api", "types", "base", "tuple"]

structTypePath :: Path.Path
structTypePath = ["api", "types", "base", "struct"]

arrayTypePath :: Path.Path
arrayTypePath = ["api", "types", "base", "array"]

getMetaType :: NodePath -> Valuespace -> CanFail MetaType
getMetaType p vs =
  do
    tp <- treeGetType p vs
    mtp <- treeGetType tp vs
    categorise mtp
  where
    categorise mtp
      | mtp == tupleTypePath = Right Tuple
      | mtp == structTypePath = Right Struct
      | mtp == arrayTypePath = Right Array
      | otherwise = Left "Weird metapath!"


globalSite = Nothing
anon = Nothing
tconst = Time 0 0

initStruct :: NodePath -> [(Path.Name, TypePath)] -> Valuespace ->
    CanFail Valuespace
initStruct root children vs =
    vs &
    treeInitNodes typePathMap &
    treeSetChildren root (fmap fst children)
  where
    prependRoot (name, tp) = (root +| name, tp)
    typePathMap = Map.fromList $ fmap prependRoot children

addConst :: [ClapiValue] -> NodePath -> Valuespace -> CanFail Valuespace
addConst cvs np = treeAdd anon IConstant cvs np globalSite tconst

getBaseValuespace :: Valuespace
getBaseValuespace = unpack (
    mempty &
    treeInitNode Path.root ["api", "types", "containers", "root"] &
    initStruct Path.root
        [("api", ["api", "types", "containers", "api"])
        ] >>=
    initStruct ["api"]
        [("types", ["api", "types", "containers", "types"]),
         ("version", ["api", "types", "self", "version"]),
         ("build", ["api", "types", "self", "build"])
        ] >>=
    initStruct ["api", "types"]
        [("base", ["api", "types", "containers", "base"]),
         ("self", ["api", "types", "containers", "self"]),
         ("containers", ["api", "types", "containers", "containers"])
        ] >>=
    initStruct ["api", "types", "base"]
        [("tuple", tupleTypePath),
         ("struct", tupleTypePath),
         ("array", tupleTypePath)
        ] >>=
    addConst (defToValues baseTupleDef) tupleTypePath >>=
    addConst (defToValues baseStructDef) structTypePath >>=
    addConst (defToValues baseArrayDef) arrayTypePath >>=
    initStruct ["api", "types", "self"]
        [("version", tupleTypePath),
         ("build", tupleTypePath)
        ] >>=
    addConst (defToValues versionDef) ["api", "types", "self", "version"] >>=
    addConst (defToValues buildDef) ["api", "types", "self", "build"] >>=
    initStruct ["api", "types", "containers"]
        [("root", structTypePath),
         ("api", structTypePath),
         ("types", structTypePath),
         ("base", structTypePath),
         ("self", structTypePath),
         ("containers", structTypePath)
        ] >>=
    addConst (defToValues rootSDef) ["api", "types", "containers", "root"] >>=
    addConst (defToValues apiSDef) ["api", "types", "containers", "api"] >>=
    addConst (defToValues typesSDef) ["api", "types", "containers", "types"] >>=
    addConst (defToValues baseSDef) ["api", "types", "containers", "base"] >>=
    addConst (defToValues selfSDef) ["api", "types", "containers", "self"] >>=
    addConst (defToValues containersSDef) ["api", "types", "containers", "containers"]
    )
  where
    unpack (Right v) = v
    versionDef = TupleDef Cannot "v" ["maj", "min"] ["noope"] []
    buildDef = TupleDef Cannot "b" ["value"] ["none here"] []
    -- FIXME: there's quite a lot of repetition here - should try to do inference
    rootSDef = StructDef Cannot "r" ["api"] [
        ["api", "types", "containers", "api"]] []
    apiSDef = StructDef Cannot "a" ["types", "version", "build"] [
        ["api", "types", "containers", "types"],
        ["api", "types", "self", "version"],
        ["api", "types", "self", "build"]] []
    typesSDef = StructDef Cannot "t" ["base", "containers", "self"] [
        ["api", "types", "containers", "base"],
        ["api", "types", "containers", "containers"],
        ["api", "types", "containers", "self"]] []
    baseSDef = StructDef Cannot "b" ["tuple", "struct", "array"] [
        tupleTypePath, tupleTypePath, tupleTypePath] []
    selfSDef = StructDef Cannot "s" ["version", "build"] [
        tupleTypePath, tupleTypePath] []
    containersSDef = StructDef Cannot "c"
        ["root", "api", "types", "self", "base", "containers"]
        (replicate 6 structTypePath) []

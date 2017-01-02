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


initStruct :: NodePath -> TypePath -> Liberty -> String ->
    [(Path.Name, TypePath)] -> Valuespace -> CanFail Valuespace
initStruct np tp lib doc children vs =
    vs &
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


globalSite = Nothing
anon = Nothing
tconst = Time 0 0

addConst :: [ClapiValue] -> NodePath -> Valuespace -> CanFail Valuespace
addConst cvs np = treeAdd anon IConstant cvs np globalSite tconst

getBaseValuespace :: Valuespace
getBaseValuespace = unpack (
    mempty &
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
    addConst (defToValues baseTupleDef) tupleTypePath >>=
    addConst (defToValues baseStructDef) structTypePath >>=
    addConst (defToValues baseArrayDef) arrayTypePath >>=
    initStruct ["api", "types", "self"]
        ["api", "types", "containers", "self"]
        Cannot "doc"
        [("version", tupleTypePath),
         ("build", tupleTypePath)
        ] >>=
    addConst (defToValues versionDef) ["api", "types", "self", "version"] >>=
    addConst (defToValues buildDef) ["api", "types", "self", "build"] >>=
    \vs -> pure
        (treeInitNode ["api", "types", "containers"] structTypePath vs) >>=
    treeSetChildren ["api", "types", "containers"]
        ["root", "api", "types", "base", "self"]
    )
  where
    unpack (Right v) = v
    unpack (Left v) = error v
    versionDef = TupleDef Cannot "v" ["maj", "min"] ["noope"] []
    buildDef = TupleDef Cannot "b" ["value"] ["none here"] []

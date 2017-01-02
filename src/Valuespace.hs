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
defToValues (ArrayDef l d t cl) = undefined
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

getBaseValuespace :: Valuespace
getBaseValuespace = unpack (
    mempty &
    treeInitNode Path.root ["api", "types", "containers", "root"] &
    initStruct Path.root
        [("api", ["api", "types", "containers", "api"])
        ] >>=
    initStruct ["api"]
        [("types", ["api", "types", "containers", "types"]),
         ("version", ["api", "types", "self", "version"])
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
    initStruct ["api", "types", "containers"]
        [("root", structTypePath),
         ("api", structTypePath),
         ("base", structTypePath),
         ("self", structTypePath),
         ("containers", structTypePath)
        ] >>=
    initStruct ["api", "types", "self"]
        [("version", tupleTypePath)] >>=
    treeAdd anon IConstant (defToValues baseTupleDef) tupleTypePath
        globalSite tconst >>=
    treeAdd anon IConstant (defToValues baseStructDef) structTypePath
        globalSite tconst >>=
    treeAdd anon IConstant (defToValues baseArrayDef) arrayTypePath
        globalSite tconst
    )
  where
    unpack (Right v) = v

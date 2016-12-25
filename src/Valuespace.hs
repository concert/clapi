module Valuespace where

import Control.Lens ((&))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Text (pack)

import qualified Path
import qualified Path.Parsing as Path
import Types (ClapiValue(..), Interpolation(..), Time(..))
import Tree (
  CanFail, (+|), ClapiTree(..), NodePath, TypePath, treeGetType, treeInitNode,
  treeInitNodes, treeAdd)

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

initStruct :: NodePath -> [Path.Name] -> [TypePath] -> Valuespace -> Valuespace
initStruct root names typePaths = treeInitNodes map
  where
    map = Map.fromList $ zip (fmap (root +|) names) typePaths

getEmptyValuespace :: Valuespace
getEmptyValuespace = unpack (
    mempty &
    treeInitNode tupleTypePath tupleTypePath &
    treeInitNode structTypePath tupleTypePath &
    treeInitNode arrayTypePath tupleTypePath &
    treeInitNode ["api", "types", "self", "version"] tupleTypePath &
    treeInitNode ["api", "version"] ["api", "types", "self", "version"] &
    initStruct ["api", "types", "containers"]
        ["root", "api", "base", "self", "containers"] (repeat structTypePath) &
    treeAdd anon IConstant (defToValues baseTupleDef) tupleTypePath
        globalSite tconst >>=
    treeAdd anon IConstant (defToValues baseStructDef) structTypePath
        globalSite tconst >>=
    treeAdd anon IConstant (defToValues baseArrayDef) arrayTypePath
        globalSite tconst
    )
  where
    unpack (Right v) = v

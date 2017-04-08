{-# LANGUAGE OverloadedStrings #-}
module Valuespace where

import Control.Monad (liftM, when)
import Control.Monad.Fail (MonadFail)
import Control.Lens ((&))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Text (Text, pack)
import Text.Printf (printf)

import qualified Path
import qualified Path.Parsing as Path
import Types (
    CanFail, eitherFail, ClapiValue(..), InterpolationType(..),
    Interpolation(..), Time(..))
import Tree (
  (+|), ClapiTree(..), NodePath, TypePath, treeGetType, treeInitNode,
  treeInitNodes, treeSetChildren, treeAdd)
import Validator (Validator, fromText, enumDesc)

unpack (Right v) = v
unpack (Left v) = error v

data Liberty = Cannot | May | Must deriving (Show, Eq, Enum, Bounded)
data Definition =
    TupleDef {
      liberty :: Liberty,
      doc :: Text,
      valueNames :: [Path.Name],
      validatorDescs :: [Text],
      validators :: [Validator],
      permittedInterpolations :: Set.Set InterpolationType}
  | StructDef {
      liberty :: Liberty,
      doc :: Text,
      childNames :: [Path.Name],
      childTypes :: [Path.Path],
      childLiberties :: [Liberty]}
  | ArrayDef {
      liberty :: Liberty,
      doc :: Text,
      childType :: Path.Path,
      childLiberty :: Liberty}

tupleDef ::
    (MonadFail m) => Liberty -> Text -> [Path.Name] -> [Text] ->
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
    (MonadFail m) => Liberty -> Text -> [Path.Name] -> [Path.Path] ->
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
    (MonadFail m) => Liberty -> Text -> Path.Path -> Liberty -> m Definition
arrayDef l d ct cl = return $ ArrayDef l d ct cl

libertyDesc = enumDesc Cannot
interpolationTypeDesc = enumDesc ITConstant
listDesc d = pack $ printf "list[%v]" d
setDesc d = pack $ printf "set[%v]" d
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
    CString $ pack $ show l,
    CString d,
    CList $ fmap (CString . pack) ns,
    CList $ fmap CString vds,
    CList $ fmap (CString . pack . show) $ Set.toList is
  ]
defToValues (StructDef l d ns ts ls) =
  [
    CString $ pack $ show l,
    CString $ d,
    CList $ fmap (CString . pack) ns,
    CList $ fmap (CString . pack . Path.toString) ts,
    CList $ fmap (CString . pack . show) ls
  ]
defToValues (ArrayDef l d t cl) =
  [
    CString $ pack $ show l,
    CString $ d,
    CString $ pack $ Path.toString t,
    CString $ pack $ show cl
  ]



data MetaType = Tuple | Struct | Array deriving (Eq, Show)
type VsTree = ClapiTree [ClapiValue]
type Vmap = Map.Map NodePath [Validator]
data Valuespace = Valuespace {getTree :: VsTree, getVmap :: Vmap}

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


initStruct :: NodePath -> TypePath -> Liberty -> Text ->
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

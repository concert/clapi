module Tree
    (
        Interpolation(..),
        TimePoint,
        TimeSeries,
        Tuple(..),
        ClapiTree(..),
        treeGet, treeAdd, treeSet, treeDelete,
        mapDiff, applyMapDiff, Delta(..)
    )
where

import Data.Word (Word32, Word64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict.Merge (
    merge, mapMissing, zipWithMatched, zipWithMaybeMatched)
import Control.Applicative (Const(..))

import Types (Name, ClapiPath(..), Time, ClapiValue)

data Interpolation = IConstant | ILinear | IBezier Word32 Word32
  deriving (Eq, Show)

type TimePoint a = (a, Interpolation)
type TimeSeries a = Map.Map Time (TimePoint a)

data Tuple = TConstant [ClapiValue] | TDynamic (TimeSeries [ClapiValue])
  deriving (Eq, Show)

data ClapiTree =
    Leaf {typePath :: ClapiPath, tuple :: Tuple} |
    Container {
        typePath :: ClapiPath,
        order :: [Name],
        subtrees :: Map.Map Name ClapiTree
    }
  deriving (Eq, Show)

treeGet :: ClapiPath -> ClapiTree -> Either String ClapiTree
treeGet path = getConst . alterTree (Const . Left) get path
  where
    get :: Maybe ClapiTree -> Const (Either String ClapiTree) (Maybe ClapiTree)
    get Nothing = Const (Left $ "Item lookup failed" ++ (show path))
    get (Just tree) = Const (Right tree)

type AlterF f a = Maybe a -> f (Maybe a)
type MakeError f a = String -> f a

treeDelete :: ClapiPath -> ClapiTree -> Either String ClapiTree
treeDelete path = alterTree Left delete path
  where
    delete :: AlterF (Either String) ClapiTree
    delete Nothing = Left $ "Tried to delete absent value at " ++ (show path)
    delete _ = Right Nothing

treeAdd :: ClapiTree -> ClapiPath -> ClapiTree -> Either String ClapiTree
treeAdd newItem path = alterTree Left add path
  where
    add :: AlterF (Either String) ClapiTree
    add Nothing = Right . Just $ newItem
    add _ = Left $ "Tried to add over present value at " ++ (show path)

treeSet :: ClapiTree -> ClapiPath -> ClapiTree -> Either String ClapiTree
treeSet replacementItem path = alterTree Left set path
  where
    set :: AlterF (Either String) ClapiTree
    set (Just tree) = Right . Just $ replacementItem
    set _ = Left $ "Tried to set at absent value at " ++ (show path)

alterTree ::
    Functor f =>
    MakeError f ClapiTree -> AlterF f ClapiTree ->
    ClapiPath -> ClapiTree -> f ClapiTree
alterTree makeError f rootPath tree = alterTree' rootPath (Just tree)
  where
    -- alterTree' :: ClapiPath -> Maybe ClapiTree -> f ClapiTree
    alterTree' _ Nothing = makeError "Intermediate lookup failed"
    alterTree' (name:path) (Just c@(Container {subtrees = items})) =
        fmap (\is -> c {subtrees = is}) (Map.alterF chooseF name items)
      where
        -- chooseF :: ClapiPath -> AlterF f ClapiTree
        chooseF = case path of
            [] -> f
            path -> \maybeTree -> fmap (Just) (alterTree' path maybeTree)
    alterTree' _ _ = makeError "Lookup failed (not a container)"


data Delta a = Remove | Add a | Change a deriving (Eq, Show)

instance Functor Delta where
    fmap f Remove = Remove
    fmap f (Add a) = Add (f a)
    fmap f (Change a) = Change (f a)

mapDiff :: (Ord k, Eq a) => Map.Map k a -> Map.Map k a -> Map.Map k (Delta a)
mapDiff m1 m2 = merge onlyInM1 onlyInM2 inBoth m1 m2
  where
    onlyInM1 = mapMissing $ \k v1 -> Remove
    onlyInM2 = mapMissing $ \k v2 -> Add v2
    inBoth = zipWithMaybeMatched diffValue
    diffValue _ x y
        | x == y = Nothing
        | otherwise = Just (Change y)

applyMapDiff :: Ord k => Map.Map k (Delta a) -> Map.Map k a -> Map.Map k a
applyMapDiff d m = merge onlyInD onlyInM inBoth d m
  where
    onlyInD = mapMissing onlyInDf
    onlyInDf _ (Add v) = v
    -- FIXME: These undefineds are bad error handling...
    onlyInDf _ _ = undefined
    onlyInM = mapMissing $ \k v -> v
    inBoth = zipWithMaybeMatched inBothf
    inBothf _ (Change v) _ = Just v
    inBothf _ (Remove) _ = Nothing


diffMaybe :: Maybe a -> Maybe a -> Maybe (Delta a)
diffMaybe (Just x) (Just y) = Just $ Change y
diffMaybe Nothing (Just y) = Just $ Add y
diffMaybe (Just x) Nothing = Just Remove
diffMaybe _ _ = Nothing

applyDiffMaybe :: Maybe (Delta a) -> Maybe a -> Maybe a
applyDiffMaybe Nothing _ = Nothing
applyDiffMaybe (Just Remove) (Just _) = Nothing
applyDiffMaybe (Just (Add y)) Nothing = Just y
applyDiffMaybe (Just (Change y)) (Just x) = Just y
applyDiffMaybe _ _ = undefined


diffEither :: Either a b -> Either a b -> Either (Delta a) (Delta b)
diffEither (Left xa) (Left ya) = Left (Change ya)
diffEither (Left xa) (Right yb) = Right (Add yb)
diffEither (Right xb) (Left ya) = Left (Add ya)
diffEither (Right xb) (Right yb) = Right (Change yb)

applyDiffEither :: Either (Delta a) (Delta b) -> Either a b -> Either a b
applyDiffEither (Left (Change a)) (Left _) = Left a
applyDiffEither (Right (Change b)) (Right _) = Right b
applyDiffEither (Left (Add a)) (Right _) = Left a
applyDiffEither (Right (Add b)) (Left _) = Right b

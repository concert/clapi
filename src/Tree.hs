{-# LANGUAGE ScopedTypeVariables #-}
module Tree
    (
        Interpolation(..),
        TimePoint,
        TimeSeries,
        Tuple(..),
        Node(..),
        ClapiTree(..),
        treeGet, treeAdd, treeSet, treeDelete,
        mapDiff, applyMapDiff, Delta(..)
    )
where

import Data.Word (Word32, Word64)
import Data.List (isPrefixOf, partition)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Map.Strict.Merge (
    merge, mapMissing, zipWithMatched, zipWithMaybeMatched)
import Control.Error.Util (hush, note)
import Control.Applicative (Const(..))

import Types (Name, ClapiPath(..), up, initLast, Time, ClapiValue)

data Interpolation = IConstant | ILinear | IBezier Word32 Word32
  deriving (Eq, Show)

type TimePoint a = (a, Interpolation)
type TimeSeries a = Map.Map Time (TimePoint a)

data Tuple = TConstant [ClapiValue] | TDynamic (TimeSeries [ClapiValue])
  deriving (Eq, Show)

data Node a =
    Leaf {typePath :: ClapiPath, leafValue :: a} |
    Container {typePath :: ClapiPath, order :: [Name]}
  deriving (Eq, Show)

type ClapiTree a = Map.Map ClapiPath (Node a)

modifyChildKeys :: ([Name] -> Either String [Name]) -> Node a ->
    Either String (Node a)
modifyChildKeys f (Leaf {}) = Left "Cannot modify child keys of leaf node"
modifyChildKeys f c@(Container {order = names}) =
    fmap (\ns -> c {order = ns}) $ f names

addChildKey :: Name -> Node a -> Either String (Node a)
addChildKey name = modifyChildKeys (\names -> Right $ name:names)

removeChildKey :: Name -> Node a -> Either String (Node a)
removeChildKey name = modifyChildKeys f
  where
    f names = note "blig" $ removeElem name names


removeElem :: Eq a => a -> [a] -> Maybe [a]
removeElem x xs = extract $ partitioned
  where
    partitioned = partition (== x) xs
    extract ([], _) = Nothing
    extract (_, xs) = Just xs


lookupMsg :: String -> ClapiPath -> ClapiTree a -> Either String (Node a)
lookupMsg msg path tree = note failMsg $ Map.lookup path tree
  where
    failMsg = msg ++ " at " ++ (show path)

treeGet :: ClapiPath -> ClapiTree a -> Either String (Node a)
treeGet = lookupMsg "Item lookup failed"

type AlterF f a = Maybe a -> f (Maybe a)
type MakeError f a = String -> f a

treeDelete :: ClapiPath -> ClapiTree a -> Either String (ClapiTree a)
treeDelete path t1 =
  do
    (ppath, name) <- note "Root path supplied to delete" $ initLast path
    -- lookupMsg "Tried to delete absent value" path tree
    parentNode <- lookupMsg "balh" ppath t1
    parentNode' <- removeChildKey name parentNode
    t2 <- return $ Map.insert ppath parentNode' t1
    return $ Map.filterWithKey predicate t2
  where
    predicate :: ClapiPath -> Node a -> Bool
    predicate k _ = not $ path `isPrefixOf` k
    -- FIXME: this is O(n): I think we could do better because the keys are
    -- ordered, and we wasted time looking up the first key!
    removeSubtree = Map.filterWithKey predicate

-- parent :: ClapiPath -> ClapiTree a -> Maybe (Node a)
-- parent path tree = hush $ treeGet (up path) tree

treeAdd :: forall a.
    Node a -> ClapiPath -> ClapiTree a -> Either String (ClapiTree a)
treeAdd newNode path t1 =
  do
    (ppath, name) <- note "Root path supplied to add" $ initLast path
    t2 <- Map.alterF (parentAdd ppath name) ppath t1
    Map.alterF add path t2
  where
    add :: AlterF (Either String) (Node a)
    add Nothing = Right . Just $ newNode
    add _ = Left $ "Tried to add over present value at " ++ (show path)
    parentAdd :: ClapiPath -> Name -> AlterF (Either String) (Node a)
    parentAdd ppath _ Nothing = Left $ "No container at " ++ (show ppath)
    parentAdd _ name (Just c) =
        fmap Just $ addChildKey name c

treeSet :: forall a.
    Node a -> ClapiPath -> ClapiTree a -> Either String (ClapiTree a)
treeSet newNode path = Map.alterF set path
  where
    set :: AlterF (Either String) (Node a)
    set (Just node) = fmap Just $ doSet node newNode
    set _ = Left $ "Tried to set at absent value at " ++ (show path)
    doSet (Leaf {typePath = t}) new@(Leaf {typePath = t'})
        | t == t' = Right new
        | otherwise = Left "Cannot change type during set"
    doSet (Container t oldOrder) new@(Container t' newOrder)
        | t == t' && Set.fromList oldOrder == Set.fromList newOrder = Right new
        | t == t' = Left "Cannot not change keys during set"
        | otherwise = Left "Cannot change type during set"
    doSet _ _ = Left "Cannot change type of container during set"


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

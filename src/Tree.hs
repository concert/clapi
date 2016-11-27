{-# LANGUAGE
    ScopedTypeVariables, TypeFamilies
#-}
module Tree
    (
        add, set, remove,
        Interpolation(..),
        TimePoint,
        TimeSeries,
        Tuple(..),
        Node(..),
        ClapiTree(..),
        emptyTree,
        treeGet, treeAdd, treeSet, treeDelete,
        mapDiff, applyMapDiff, Delta(..),
        add', set', remove',
        failAt,
    )
where

import Data.Word (Word32, Word64)
import Data.List (isPrefixOf, partition)
import Data.Bifunctor (Bifunctor, bimap)
import Data.Bifoldable (Bifoldable, bifoldMap, binull)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Map.Strict.Merge (
    merge, mapMissing, zipWithMatched, zipWithMaybeMatched)
import Control.Lens (
    Lens', view, at, At(..), Index(..), IxValue(..), Ixed(..))
import Control.Error.Util (hush, note)
import Control.Applicative (Const(..))

import Types (Name, ClapiPath(..), root, up, initLast, Time, ClapiValue)


type CanFail a = Either String a
type AlterF a = Maybe a -> CanFail (Maybe a)

idAlter :: AlterF a
idAlter Nothing = Right Nothing
idAlter (Just x) = Right $ Just x

add' :: a -> AlterF a
add' new Nothing = Right . Just $ new
add' _ (Just _) = Left "tried to overwrite"

add :: (At m) => IxValue m -> Index m -> m -> CanFail (m)
add a k = at k (add' a)

set' :: a -> AlterF a
set' _ Nothing = Left "tried to set absent value"
set' new (Just _) = Right . Just $ new

set :: (At m) => IxValue m -> Index m -> m -> CanFail (m)
set a k = at k (set' a)

remove' :: AlterF a
remove' Nothing = Left "tried to remove absent value"
remove' (Just _) = Right Nothing

remove :: (At m) => Index m -> m -> CanFail (m)
remove k = at k remove'

data Interpolation = IConstant | ILinear | IBezier Word32 Word32
  deriving (Eq, Show)

type TimePoint a = (Interpolation, a)
type TimeSeries a = Map.Map Time a

data Tuple a = TConstant (Maybe a) | TDynamic (TimeSeries a)
  deriving (Eq, Show)

type instance Index (Tuple a) = Maybe Time
type instance IxValue (Tuple a) = a
instance Ixed (Tuple a) where
  ix Nothing f tup@(TConstant ma) = case ma of
      Nothing -> pure tup
      (Just a) -> (\v -> TConstant $ Just v) <$> f a
  ix (Just time) f tup@(TDynamic ts) = case Map.lookup time ts of
      Nothing -> pure tup
      (Just a) -> (\v -> TDynamic $ Map.insert time v ts) <$> f a
  ix _ _ tup = pure tup

instance At (Tuple a) where
    at (Nothing) f (TConstant ma) = TConstant <$> f ma
    at (Just time) f (TDynamic tsa) = TDynamic <$> Map.alterF f time tsa
    at _ _ _ = undefined

class Ixed m => FailAt m where
    failAt :: Index m -> (Maybe (IxValue m) -> CanFail (Maybe (IxValue m))) ->
        m -> CanFail m

instance FailAt (Tuple a) where
    failAt Nothing f tup@(TConstant _) = at Nothing f tup
    failAt (Just _) f (TConstant _) =
        Left "Can't provide time to index constant time series"
    failAt t@(Just _) f tup@(TDynamic _) = at t f tup
    failAt _ _ _ = Left "Must provide a time to index dynamic time series"


instance Functor Tuple where
    fmap f (TConstant maybeVs) = TConstant $ fmap f maybeVs
    fmap f (TDynamic tsVs) = TDynamic $ fmap f tsVs

instance Foldable Tuple where
    foldMap f (TConstant maybeVs) = foldMap f maybeVs
    foldMap f (TDynamic tsVs) = foldMap f tsVs

data Node a b =
    Leaf {typePath :: ClapiPath, leafValue :: Tuple b} |
    Container {typePath :: ClapiPath, order :: [a]}
  deriving (Eq, Show)

instance Functor (Node a) where
    fmap f l@(Leaf {leafValue = v}) = l {leafValue = fmap f v}
    fmap _ (Container tp ord) = Container tp ord

instance Foldable (Node a) where
    foldMap f (Leaf {leafValue = v}) = foldMap f v
    foldMap f (Container {}) = mempty

instance Bifunctor Node where
    bimap f _ c@(Container {order = ord}) = c {order = fmap f ord}
    bimap _ g l@(Leaf {leafValue = v}) = l {leafValue = fmap g v}

instance Bifoldable Node where
    bifoldMap f _ (Container {order = ord}) = foldMap f ord
    bifoldMap _ g (Leaf {leafValue = v}) = foldMap g v

type ClapiTree a = Map.Map ClapiPath (Node Name a)

emptyTree :: ClapiTree a
emptyTree = Map.singleton root (Container [] [])

modifyChildKeys :: ([a] -> CanFail [a]) -> Node a b -> CanFail (Node a b)
modifyChildKeys f (Leaf {}) = Left "Cannot modify child keys of leaf node"
modifyChildKeys f c@(Container {order = names}) =
    fmap (\ns -> c {order = ns}) $ f names

addChildKey :: Eq a => a -> Node a b -> CanFail (Node a b)
addChildKey name = modifyChildKeys (\names -> Right $ name:names)

removeChildKey :: Eq a => a -> Node a b -> CanFail (Node a b)
removeChildKey name = modifyChildKeys f
  where
    f names = note "Tried to remove absent child key" $ removeElem name names


removeElem :: Eq a => a -> [a] -> Maybe [a]
removeElem x xs = extract $ partitioned
  where
    partitioned = partition (== x) xs
    extract ([], _) = Nothing
    extract (_, xs) = Just xs


lookupMsg :: String -> ClapiPath -> ClapiTree a -> CanFail (Node Name a)
lookupMsg msg path tree = note failMsg $ Map.lookup path tree
  where
    failMsg = msg ++ " at " ++ (show path)

treeGet :: ClapiPath -> ClapiTree a -> CanFail (Node Name a)
treeGet path tree = note "Item lookup failed" $ view (at path) tree

treeDelete :: ClapiPath -> ClapiTree a -> CanFail (ClapiTree a)
treeDelete path t1 =
  do
    (ppath, name) <- note "Tried to delete root path" $ initLast path
    -- lookupMsg "Tried to delete absent value" path tree
    parentNode <- lookupMsg "Deletion path not found" ppath t1
    parentNode' <- removeChildKey name parentNode
    t2 <- return $ Map.insert ppath parentNode' t1
    return $ Map.filterWithKey predicate t2
  where
    predicate :: ClapiPath -> Node Name a -> Bool
    predicate k _ = not $ path `isPrefixOf` k
    -- FIXME: this is O(n): I think we could do better because the keys are
    -- ordered, and we wasted time looking up the first key!
    removeSubtree = Map.filterWithKey predicate

treeAdd :: forall a.
    Node Name a -> ClapiPath -> ClapiTree a -> CanFail (ClapiTree a)
treeAdd newNode path t1 =
  do
    (ppath, name) <- note "Root path supplied to add" $ initLast path
    t2 <- at ppath (parentAdd ppath name) t1
    at path add t2
  where
    add :: AlterF (Node Name a)
    add Nothing = Right . Just $ newNode
    add _ = Left $ "Tried to add over present value at " ++ (show path)
    parentAdd :: ClapiPath -> Name -> AlterF (Node Name a)
    parentAdd ppath _ Nothing = Left $ "No container at " ++ (show ppath)
    parentAdd _ name (Just c) =
        fmap Just $ addChildKey name c

treeSet :: forall a.
    Node Name a -> ClapiPath -> ClapiTree a -> CanFail (ClapiTree a)
treeSet newNode path = at path set
  where
    set :: AlterF (Node Name a)
    set (Just node) = fmap Just $ doSet node newNode
    set _ = Left $ "Tried to set at absent value at " ++ (show path)
    doSet (Leaf {typePath = t}) new@(Leaf {typePath = t'})
        | t == t' = Right new
        | otherwise = Left "Cannot change type during set"
    doSet (Container t oldOrder) new@(Container t' newOrder)
        | t == t' && Set.fromList oldOrder == Set.fromList newOrder = Right new
        | t == t' = Left "Cannot change keys during set"
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

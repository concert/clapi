{-# LANGUAGE
    DeriveFoldable
#-}

module Data.Map.Mos
  ( Mos, unMos
  , singleton, singletonSet
  , fromMap, fromFoldable, fromList, invertMap
  , keysSet, hasKey, member, contains
  , invert
  , toList, toSet, valueSet
  , insert, insertSet, replaceSet, delete, deleteSet, deleteLookupSet
  , remove, removeSet
  , lookup
  , difference, intersection, union
  , filter, filterWithKey, filterKey
  , partition, partitionWithKey, partitionKey
  , map, mapWithKey
  ) where

import Prelude hiding (lookup, map, filter)

import Data.Bifunctor (bimap)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Merge.Strict (
    merge, preserveMissing, dropMissing, zipWithMaybeMatched)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)

import qualified Data.Maybe.Clapi as Maybe
import qualified Data.Map.Clapi as Map

-- We don't export this constructor so that we can guarantee that if the final
-- element is removed from a set, we remove the set from the map:
newtype Mos k a
  = Mos
  { unMos :: Map k (Set a)
  } deriving (Show, Eq, Ord, Foldable)

instance (Ord k, Ord a) => Semigroup (Mos k a) where
  Mos m1 <> Mos m2 = Mos $ Map.unionWith (<>) m1 m2
instance (Ord k, Ord a) => Monoid (Mos k a) where
  mempty = Mos mempty
  mappend = (<>)


singleton :: k -> a -> Mos k a
singleton k a = Mos $ Map.singleton k $ Set.singleton a

singletonSet :: Ord k => k -> Set a -> Mos k a
singletonSet k sa = Mos $ if null sa then mempty else Map.singleton k sa

fromMap :: Map k (Set a) -> Mos k a
fromMap = Mos . Map.filter (not . null)

fromFoldable :: (Ord k, Ord a, Foldable f) => f (k, a) -> Mos k a
fromFoldable = foldl' (\mos (k, a) -> insert k a mos) mempty

fromList :: (Ord k, Ord a) => [(k, a)] -> Mos k a
fromList = fromFoldable

invertMap :: (Ord k, Ord a) => Map k a -> Mos a k
invertMap = Map.foldrWithKey (flip insert) mempty

keysSet :: Mos k a -> Set k
keysSet = Map.keysSet . unMos

hasKey :: Ord k => Mos k a -> k -> Bool
hasKey (Mos m) k = k `Map.member` m

member :: Ord a => a -> Mos k a -> Bool
member a = any (a `Set.member`) . unMos

contains :: (Ord k, Ord a) => k -> a -> Mos k a -> Bool
contains k a = maybe False (a `Set.member`) . Map.lookup k . unMos

invert :: (Ord k, Ord a) => Mos k a -> Mos a k
invert = fromFoldable . Set.map swap . toSet

_toLos :: Mos k a -> [Set (k, a)]
_toLos = Map.elems . Map.mapWithKey (\k as -> Set.mapMonotonic (k,) as) . unMos

toList :: Mos k a -> [(k, a)]
toList = foldMap Set.toList . _toLos

toSet :: (Ord k, Ord a) => Mos k a -> Set (k, a)
toSet = foldMap id . _toLos

valueSet :: Ord a => Mos k a -> Set a
valueSet = foldMap id . unMos

insert :: (Ord k, Ord a) => k -> a -> Mos k a -> Mos k a
insert k a = Mos . Map.updateM (Set.insert a) k . unMos

insertSet :: (Ord k, Ord a) => k -> Set a -> Mos k a -> Mos k a
insertSet k sa = Mos . Map.updateM (Set.union sa) k . unMos

replaceSet :: Ord k => k -> Set a -> Mos k a -> Mos k a
replaceSet k sa = Mos . Map.insert k sa . unMos

delete :: (Ord k, Ord a) => k -> a -> Mos k a -> Mos k a
delete k a = Mos . Map.update f k . unMos
  where
    f = Maybe.fromFoldable . (Set.delete a)

deleteSet :: Ord k => k -> Mos k a -> Mos k a
deleteSet k = Mos . Map.delete k . unMos

deleteLookupSet :: (Ord k, Ord a) => k -> Mos k a -> (Set a, Mos k a)
deleteLookupSet k (Mos m) =
  let
    (s, m') = Map.updateLookupWithKey (\_ _ -> Nothing) k m
  in
    (fromMaybe mempty s, Mos m')

remove :: (Ord k, Ord a) => a -> Mos k a -> Mos k a
remove a = Mos . Map.mapMaybe (Maybe.fromFoldable . (Set.delete a)) . unMos

removeSet :: Ord a => Set a -> Mos k a -> Mos k a
removeSet as = Mos . Map.filter (not . null) . fmap (`Set.difference` as) . unMos

lookup :: (Ord k, Ord a) => k -> Mos k a -> Set a
lookup k = Map.findWithDefault mempty k . unMos

difference :: (Ord k, Ord a) => Mos k a -> Mos k a -> Mos k a
difference (Mos m1) (Mos m2) = Mos $
    merge preserveMissing dropMissing (zipWithMaybeMatched f) m1 m2
  where
    f _k sa1 sa2 = Maybe.fromFoldable $ Set.difference sa1 sa2

intersection :: (Ord k, Ord a) => Mos k a -> Mos k a -> Mos k a
intersection (Mos m1) (Mos m2) = Mos $
    merge dropMissing dropMissing (zipWithMaybeMatched f) m1 m2
  where
    f _k sa1 sa2 = Maybe.fromFoldable $ Set.intersection sa1 sa2

union :: (Ord k, Ord a) => Mos k a -> Mos k a -> Mos k a
union (Mos m1) (Mos m2) = Mos $ Map.unionM m1 m2

filterWithKey :: (k -> a -> Bool) -> Mos k a -> Mos k a
filterWithKey f = Mos
  . Map.mapMaybeWithKey (\k sa -> Maybe.fromFoldable $ Set.filter (f k) sa)
  . unMos

filter :: (a -> Bool) -> Mos k a -> Mos k a
filter f = filterWithKey $ const f

filterKey :: (k -> Bool) -> Mos k a -> Mos k a
filterKey f = Mos . Map.filterWithKey (\k _a -> f k) . unMos

partitionWithKey :: Ord k => (k -> a -> Bool) -> Mos k a -> (Mos k a, Mos k a)
partitionWithKey f =
    split . Map.mapWithKey (\k sa -> Set.partition (f k) sa) . unMos
  where
    mkMos = Mos . Map.filter (not . null)
    split m = (mkMos $ fst <$> m, mkMos $ snd <$> m)

partition :: Ord k => (a -> Bool) -> Mos k a -> (Mos k a, Mos k a)
partition f = partitionWithKey (\_ a -> f a)

partitionKey :: Ord k => (k -> Bool) -> Mos k a -> (Mos k a, Mos k a)
partitionKey f = bimap Mos Mos . Map.partitionWithKey (\k _ -> f k) . unMos

mapWithKey :: Ord b => (k -> a -> b) -> Mos k a -> Mos k b
mapWithKey f = Mos . Map.mapWithKey (\k -> Set.map $ \a -> f k a) . unMos

map :: Ord b => (a -> b) -> Mos k a -> Mos k b
map f = mapWithKey (\_ a -> f a)

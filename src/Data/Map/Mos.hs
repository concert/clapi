module Data.Map.Mos where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map.Strict.Merge (
    merge, preserveMissing, dropMissing, zipWithMatched, zipWithMaybeMatched)

import qualified Data.Maybe.Clapi as Maybe
import qualified Data.Map.Clapi as Map

type Mos k a = Map.Map k (Set.Set a)

insert :: (Ord k, Ord a) => k -> a -> Mos k a -> Mos k a
insert k a = Map.updateM (Set.insert a) k

delete :: (Ord k, Ord a) => k -> a -> Mos k a -> Mos k a
delete k a = Map.update f k
  where
    f = Maybe.fromFoldable . (Set.delete a)

invertMap :: (Ord k, Ord a) => Map.Map k a -> Mos a k
invertMap = Map.foldrWithKey (flip insert) mempty

difference :: (Ord k, Ord a) => Mos k a -> Mos k a -> Mos k a
difference = merge preserveMissing dropMissing (zipWithMaybeMatched f)
  where
    f k sa1 sa2 = Maybe.fromFoldable $ Set.difference sa1 sa2

union :: (Ord k, Ord a) => Mos k a -> Mos k a -> Mos k a
union = merge preserveMissing preserveMissing (zipWithMatched f)
  where
    f k sa1 sa2 = Set.union sa1 sa2

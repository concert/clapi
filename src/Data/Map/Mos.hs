{-# OPTIONS_GHC -Wall -Wno-orphans #-}

module Data.Map.Mos where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map.Strict.Merge (
    merge, preserveMissing, dropMissing, zipWithMaybeMatched)

import qualified Data.Maybe.Clapi as Maybe
import qualified Data.Map.Clapi as Map

type Mos k a = Map.Map k (Set.Set a)

insert :: (Ord k, Ord a) => k -> a -> Mos k a -> Mos k a
insert k a = Map.updateM (Set.insert a) k

delete :: (Ord k, Ord a) => k -> a -> Mos k a -> Mos k a
delete k a = Map.update f k
  where
    f = Maybe.fromFoldable . (Set.delete a)

remove :: (Ord k, Ord a) => a -> Mos k a -> Mos k a
remove a = Map.mapMaybe (Maybe.fromFoldable . (Set.delete a))

invertMap :: (Ord k, Ord a) => Map.Map k a -> Mos a k
invertMap = Map.foldrWithKey (flip insert) mempty

toSet :: (Ord k, Ord a) => Mos k a -> Set.Set (k, a)
toSet mos = foldMap id $ Map.mapWithKey (\k as -> Set.map ((,) k) as) mos

difference :: (Ord k, Ord a) => Mos k a -> Mos k a -> Mos k a
difference = merge preserveMissing dropMissing (zipWithMaybeMatched f)
  where
    f _k sa1 sa2 = Maybe.fromFoldable $ Set.difference sa1 sa2

union :: (Ord k, Ord a) => Mos k a -> Mos k a -> Mos k a
union = Map.unionM

concat :: (Ord k, Ord a) => [Mos k a] -> Mos k a
concat = foldr union mempty

partition :: Ord k => (a -> Bool) -> Mos k a -> (Mos k a, Mos k a)
partition f =
    Map.foldlWithKey
      (\(tmos, fmos) k (tset, fset) -> (ins k tset tmos, ins k fset fmos))
      mempty
    . fmap (Set.partition f)
  where
    ins k s mos = if null s then mos else Map.insert k s mos


type Dependencies k a = (Map.Map k a, Mos a k)

dependenciesFromMap :: (Ord k, Ord a) => Map.Map k a -> Dependencies k a
dependenciesFromMap m = (m, invertMap m)

getDependency :: (Ord k, Ord a) => k -> Dependencies k a -> Maybe a
getDependency k = Map.lookup k . fst

getDependants :: (Ord k, Ord a) => a -> Dependencies k a -> Set.Set k
getDependants a = maybe mempty id . Map.lookup a . snd

allDependencies :: Dependencies k a -> Set.Set a
allDependencies = Map.keysSet . snd

allDependants :: Dependencies k a -> Set.Set k
allDependants = Map.keysSet . fst

setDependency
  :: (Ord k, Ord a) => k -> a -> Dependencies k a -> Dependencies k a
setDependency k a (deps, revDeps) = (deps', insert a k $ revDeps' mCurA)
  where
    f _ newA _curA = newA
    (mCurA, deps') = Map.insertLookupWithKey f k a deps
    revDeps' (Just curA) = delete curA k revDeps
    revDeps' Nothing = revDeps

setDependencies ::
    (Ord k, Ord a) => Map.Map k a -> Dependencies k a -> Dependencies k a
setDependencies newDs ds = foldr (uncurry setDependency) ds $ Map.toList newDs

delDependency :: (Ord k, Ord a) => k -> Dependencies k a -> Dependencies k a
delDependency k (deps, revDeps) = (deps', revDeps' ma)
  where
    f _ _ = Nothing
    (ma, deps') = Map.updateLookupWithKey f k deps
    revDeps' (Just a) = delete a k revDeps
    revDeps' Nothing = revDeps

delDependencies ::
    (Ord k, Ord a, Foldable f) => f k -> Dependencies k a -> Dependencies k a
delDependencies ks ds = foldr delDependency ds ks

filterDeps
  :: (Ord k, Ord a) => (k -> a -> Bool) -> Dependencies k a -> Dependencies k a
filterDeps f (deps, revDeps) =
  let
    (toKeep, toDrop) = Map.partitionWithKey f deps
  in
    (toKeep, flip Set.difference (Map.keysSet toDrop) <$> revDeps)

filterDependencies
  :: (Ord k, Ord a) => (k -> Bool) -> Dependencies k a -> Dependencies k a
filterDependencies f = filterDeps (\k _ -> f k)

filterDependents
  :: (Ord k, Ord a) => (a -> Bool) -> Dependencies k a -> Dependencies k a
filterDependents f = filterDeps (const f)


type Dependencies' k a = (Mos k a, Mos a k)

setDependencies' ::
    (Ord k, Ord a) => k -> [a] -> Dependencies' k a -> Dependencies' k a
setDependencies' _ [] ds = ds
setDependencies' k as (deps, revDeps) = (deps', revDeps'')
  where
    f _ newAs _curAs = newAs
    (mas, deps') = Map.insertLookupWithKey f k (Set.fromList as) deps
    revDeps' (Just as') = foldr (\a -> delete a k) revDeps as'
    revDeps' Nothing = revDeps
    revDeps'' = foldr (\a -> insert a k) (revDeps' mas) as

delDependencies' ::
    (Ord k, Ord a) => k -> Dependencies' k a -> Dependencies' k a
delDependencies' k (deps, revDeps) = (deps', revDeps' mas)
  where
    f _ _ = Nothing
    (mas, deps') = Map.updateLookupWithKey f k deps
    revDeps' (Just as) = foldr (\a -> delete a k) revDeps as
    revDeps' Nothing = revDeps

getDependants' ::
  (Ord k, Ord a) => a -> Dependencies' k a -> Set.Set k
getDependants' a = Maybe.toMonoid . Map.lookup a . snd

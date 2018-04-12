{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE TupleSections #-}

module Data.Map.Mos where

import qualified Data.Map as Map
import Data.Map.Strict.Merge (
    merge, preserveMissing, dropMissing, zipWithMaybeMatched)
import qualified Data.Set as Set
import Data.Tuple (swap)

import qualified Data.Maybe.Clapi as Maybe
import qualified Data.Map.Clapi as Map

type Mos k a = Map.Map k (Set.Set a)

fromFoldable :: (Ord k, Ord a, Foldable f) => f (k, a) -> Mos k a
fromFoldable = foldr (uncurry insert) mempty

fromList :: (Ord k, Ord a) => [(k, a)] -> Mos k a
fromList = fromFoldable

toList :: Mos k a -> [(k, a)]
toList = mconcat . Map.elems . Map.mapWithKey (\k as -> (k,) <$> Set.toList as)

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

invert :: (Ord k, Ord a) => Mos k a -> Mos a k
invert = fromFoldable . Set.map swap . toSet

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

dependenciesFromList :: (Ord k, Ord a) => [(k, a)] -> Dependencies k a
dependenciesFromList = dependenciesFromMap . Map.fromList

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

nonNull :: Foldable f => f a -> Maybe (f a)
nonNull s = if null s then Nothing else Just s

filterDeps
  :: (Ord k, Ord a) => (k -> a -> Bool) -> Dependencies k a -> Dependencies k a
filterDeps f (deps, revDeps) =
  let
    (toKeep, toDrop) = Map.partitionWithKey f deps
  in
    (toKeep, Map.mapMaybe (nonNull . flip Set.difference (Map.keysSet toDrop)) revDeps)

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

setDependenciesFromMos'
  :: (Ord k, Ord a) => Mos k a -> Dependencies' k a -> Dependencies' k a
setDependenciesFromMos' mos (fwd, rev) = (union mos fwd, union rev $ invert mos)

setRevDependenciesFromMos'
  :: (Ord k, Ord a) => Mos a k -> Dependencies' k a -> Dependencies' k a
setRevDependenciesFromMos' mos (fwd, rev) =
  (union fwd $ invert mos, union mos rev)

delDependencies' ::
    (Ord k, Ord a) => k -> Dependencies' k a -> Dependencies' k a
delDependencies' k (deps, revDeps) = (deps', revDeps' mas)
  where
    f _ _ = Nothing
    (mas, deps') = Map.updateLookupWithKey f k deps
    revDeps' (Just as) = foldr (\a -> delete a k) revDeps as
    revDeps' Nothing = revDeps

filterDependencies'
  :: (Ord k, Ord a) => (k -> Bool) -> Dependencies' k a -> Dependencies' k a
filterDependencies' f (fwd, rev) =
  let
    (discardFwd, keepFwd) = Map.partitionWithKey (\k _ -> f k) fwd
    keepRev = Map.filter (not . null) $
      fmap (flip Set.difference $ Map.keysSet discardFwd) rev
  in
    (keepFwd, keepRev)

getDependants' ::
  (Ord k, Ord a) => a -> Dependencies' k a -> Set.Set k
getDependants' a = Maybe.toMonoid . Map.lookup a . snd

module Data.Map.Mos
  ( Mos, unMos
  , singleton, singletonSet
  , fromFoldable, fromList, invertMap
  , invert
  , toList, toSet, valueSet
  , insert, insertSet, delete, deleteSet, remove, removeSet
  , lookup
  , difference, union
  , partition, partitionWithKey

  , Dependencies, fwdDeps, revDeps
  , dependenciesFromMap, dependenciesFromList
  , getDependency, getDependants
  , allDependencies, allDependants
  , setDependency, setDependencies
  , delDependency, delDependencies
  , filterDependencies, filterDependents
  ) where

import Prelude hiding (lookup)

import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Strict.Merge (
    merge, preserveMissing, dropMissing, zipWithMaybeMatched)
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)

import qualified Data.Maybe.Clapi as Maybe
import qualified Data.Map.Clapi as Map

newtype Mos k a = Mos { unMos :: Map k (Set a) } deriving (Show, Eq)

instance (Ord k, Ord a) => Monoid (Mos k a) where
  mempty = Mos mempty
  (Mos m1) `mappend` (Mos m2) = Mos $ Map.unionWith (<>) m1 m2

singleton :: k -> a -> Mos k a
singleton k a = Mos $ Map.singleton k $ Set.singleton a

singletonSet :: Ord k => k -> Set a -> Mos k a
singletonSet k sa = Mos $ if null sa then mempty else Map.singleton k sa

fromFoldable :: (Ord k, Ord a, Foldable f) => f (k, a) -> Mos k a
fromFoldable = foldl' (\mos (k, a) -> insert k a mos) mempty

fromList :: (Ord k, Ord a) => [(k, a)] -> Mos k a
fromList = fromFoldable

invertMap :: (Ord k, Ord a) => Map k a -> Mos a k
invertMap = Map.foldrWithKey (flip insert) mempty

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

delete :: (Ord k, Ord a) => k -> a -> Mos k a -> Mos k a
delete k a = Mos . Map.update f k . unMos
  where
    f = Maybe.fromFoldable . (Set.delete a)

deleteSet :: Ord k => k -> Mos k a -> Mos k a
deleteSet k = Mos . Map.delete k . unMos

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

union :: (Ord k, Ord a) => Mos k a -> Mos k a -> Mos k a
union (Mos m1) (Mos m2) = Mos $ Map.unionM m1 m2

partitionWithKey :: Ord k => (k -> a -> Bool) -> Mos k a -> (Mos k a, Mos k a)
partitionWithKey f =
    split . Map.mapWithKey (\k sa -> Set.partition (f k) sa) . unMos
  where
    split m = (Mos $ fst <$> m, Mos $ snd <$> m)

partition :: Ord k => (a -> Bool) -> Mos k a -> (Mos k a, Mos k a)
partition f = partitionWithKey (\_ a -> f a)


data Dependencies k a
  = Dependencies { fwdDeps :: Map k a, revDeps :: Mos a k }
  deriving (Show, Eq)

instance (Ord k, Ord a) => Monoid (Dependencies k a) where
  mempty = Dependencies mempty mempty
  (Dependencies f1 r1) `mappend` (Dependencies f2 r2) =
    Dependencies (f1 <> f2) (r1 <> r2)

dependenciesFromMap :: (Ord k, Ord a) => Map k a -> Dependencies k a
dependenciesFromMap m = Dependencies m $ invertMap m

dependenciesFromList :: (Ord k, Ord a) => [(k, a)] -> Dependencies k a
dependenciesFromList = dependenciesFromMap . Map.fromList

getDependency :: (Ord k, Ord a) => k -> Dependencies k a -> Maybe a
getDependency k = Map.lookup k . fwdDeps

getDependants :: (Ord k, Ord a) => a -> Dependencies k a -> Set k
getDependants a = maybe mempty id . Map.lookup a . unMos . revDeps

allDependencies :: Dependencies k a -> Set a
allDependencies = Map.keysSet . unMos . revDeps

allDependants :: Dependencies k a -> Set k
allDependants = Map.keysSet . fwdDeps

setDependency
  :: (Ord k, Ord a) => k -> a -> Dependencies k a -> Dependencies k a
setDependency k a (Dependencies deps rDeps) =
    Dependencies deps' (insert a k $ rDeps' mCurA)
  where
    f _ newA _curA = newA
    (mCurA, deps') = Map.insertLookupWithKey f k a deps
    rDeps' (Just curA) = delete curA k rDeps
    rDeps' Nothing = rDeps

setDependencies ::
    (Ord k, Ord a) => Map k a -> Dependencies k a -> Dependencies k a
setDependencies newDs ds = foldr (uncurry setDependency) ds $ Map.toList newDs

delDependency :: (Ord k, Ord a) => k -> Dependencies k a -> Dependencies k a
delDependency k (Dependencies deps rDeps) = Dependencies deps' (rDeps' ma)
  where
    f _ _ = Nothing
    (ma, deps') = Map.updateLookupWithKey f k deps
    rDeps' (Just a) = delete a k rDeps
    rDeps' Nothing = rDeps

delDependencies ::
    (Ord k, Ord a, Foldable f) => f k -> Dependencies k a -> Dependencies k a
delDependencies ks ds = foldr delDependency ds ks

filterDeps
  :: (Ord k, Ord a) => (k -> a -> Bool) -> Dependencies k a -> Dependencies k a
filterDeps f (Dependencies deps rDeps) =
  let
    (toKeepFwd, toDropFwd) = Map.partitionWithKey f deps
    toKeepRev = removeSet (Map.keysSet toDropFwd) rDeps
  in
    Dependencies toKeepFwd toKeepRev

filterDependencies
  :: (Ord k, Ord a) => (k -> Bool) -> Dependencies k a -> Dependencies k a
filterDependencies f = filterDeps (\k _ -> f k)

filterDependents
  :: (Ord k, Ord a) => (a -> Bool) -> Dependencies k a -> Dependencies k a
filterDependents f = filterDeps (const f)

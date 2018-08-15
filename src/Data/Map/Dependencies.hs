module Data.Map.Dependencies
  ( Dependencies, fwdDeps, revDeps
  , dependenciesFromMap, dependenciesFromList
  , getDependency, getDependants
  , allDependencies, allDependants
  , setDependency, setDependencies
  , delDependency, delDependencies
  , filterDependencies, filterDependents, filterDeps
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Set (Set)

import Data.Map.Mos (Mos, unMos)
import qualified Data.Map.Mos as Mos

data Dependencies k a
  = Dependencies { fwdDeps :: Map k a, revDeps :: Mos a k }
  deriving (Show, Eq)

instance (Ord k, Ord a) => Monoid (Dependencies k a) where
  mempty = Dependencies mempty mempty
  (Dependencies f1 r1) `mappend` (Dependencies f2 r2) =
    Dependencies (f1 <> f2) (r1 <> r2)

dependenciesFromMap :: (Ord k, Ord a) => Map k a -> Dependencies k a
dependenciesFromMap m = Dependencies m $ Mos.invertMap m

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
    Dependencies deps' (Mos.insert a k $ rDeps' mCurA)
  where
    f _ newA _curA = newA
    (mCurA, deps') = Map.insertLookupWithKey f k a deps
    rDeps' (Just curA) = Mos.delete curA k rDeps
    rDeps' Nothing = rDeps

setDependencies ::
    (Ord k, Ord a) => Map k a -> Dependencies k a -> Dependencies k a
setDependencies newDs ds = foldr (uncurry setDependency) ds $ Map.toList newDs

delDependency :: (Ord k, Ord a) => k -> Dependencies k a -> Dependencies k a
delDependency k (Dependencies deps rDeps) = Dependencies deps' (rDeps' ma)
  where
    f _ _ = Nothing
    (ma, deps') = Map.updateLookupWithKey f k deps
    rDeps' (Just a) = Mos.delete a k rDeps
    rDeps' Nothing = rDeps

delDependencies ::
    (Ord k, Ord a, Foldable f) => f k -> Dependencies k a -> Dependencies k a
delDependencies ks ds = foldr delDependency ds ks

filterDeps
  :: (Ord k, Ord a) => (k -> a -> Bool) -> Dependencies k a -> Dependencies k a
filterDeps f (Dependencies deps rDeps) =
  let
    (toKeepFwd, toDropFwd) = Map.partitionWithKey f deps
    toKeepRev = Mos.removeSet (Map.keysSet toDropFwd) rDeps
  in
    Dependencies toKeepFwd toKeepRev

filterDependencies
  :: (Ord k, Ord a) => (k -> Bool) -> Dependencies k a -> Dependencies k a
filterDependencies f = filterDeps (\k _ -> f k)

filterDependents
  :: (Ord k, Ord a) => (a -> Bool) -> Dependencies k a -> Dependencies k a
filterDependents f = filterDeps (const f)

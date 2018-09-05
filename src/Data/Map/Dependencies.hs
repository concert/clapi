module Data.Map.Dependencies
  ( Dependencies, fwdDeps, revDeps
  , singleton
  , fromMap, fromList
  , keysSet, keysSetR
  , lookup, lookupRev
  , allDependencies, allDependants
  , insert, insertLookupWithKey, setDependencies
  , delDependency, delDependencies
  , filterWithKey, filterKey, filter
  , partitionWithKey, partitionKey, partition
  ) where

import Prelude hiding (lookup, filter)

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

instance Foldable (Dependencies k) where
  foldr f acc = foldr f acc . fwdDeps

singleton :: (Ord k, Ord a) => k -> a -> Dependencies k a
singleton k a = fromMap $ Map.singleton k a

fromMap :: (Ord k, Ord a) => Map k a -> Dependencies k a
fromMap m = Dependencies m $ Mos.invertMap m

fromList :: (Ord k, Ord a) => [(k, a)] -> Dependencies k a
fromList = fromMap . Map.fromList

keysSet :: Dependencies k a -> Set k
keysSet = Map.keysSet . fwdDeps

keysSetR :: Dependencies k a -> Set a
keysSetR = Mos.keysSet . revDeps

lookup :: (Ord k, Ord a) => k -> Dependencies k a -> Maybe a
lookup k = Map.lookup k . fwdDeps

lookupRev :: (Ord k, Ord a) => a -> Dependencies k a -> Set k
lookupRev a = maybe mempty id . Map.lookup a . unMos . revDeps

allDependencies :: Dependencies k a -> Set a
allDependencies = Map.keysSet . unMos . revDeps

allDependants :: Dependencies k a -> Set k
allDependants = Map.keysSet . fwdDeps

insertLookupWithKey
  :: (Ord k, Ord a) => k -> a -> Dependencies k a -> (Maybe a, Dependencies k a)
insertLookupWithKey k a (Dependencies deps rDeps) =
    (mCurA, Dependencies deps' $ Mos.insert a k rDeps')
  where
    f _ newA _curA = newA
    (mCurA, deps') = Map.insertLookupWithKey f k a deps
    rDeps' = maybe rDeps (\curA -> Mos.delete curA k rDeps) mCurA

insert
  :: (Ord k, Ord a) => k -> a -> Dependencies k a -> Dependencies k a
insert k a = snd . insertLookupWithKey k a

setDependencies ::
    (Ord k, Ord a) => Map k a -> Dependencies k a -> Dependencies k a
setDependencies newDs ds = foldr (uncurry insert) ds $ Map.toList newDs

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

filterWithKey
  :: (Ord k, Ord a) => (k -> a -> Bool) -> Dependencies k a -> Dependencies k a
filterWithKey f (Dependencies deps rDeps) =
  let
    (toKeepFwd, toDropFwd) = Map.partitionWithKey f deps
    toKeepRev = Mos.removeSet (Map.keysSet toDropFwd) rDeps
  in
    Dependencies toKeepFwd toKeepRev

filterKey
  :: (Ord k, Ord a) => (k -> Bool) -> Dependencies k a -> Dependencies k a
filterKey f = filterWithKey (\k _ -> f k)

filter :: (Ord k, Ord a) => (a -> Bool) -> Dependencies k a -> Dependencies k a
filter f = filterWithKey (const f)

partitionWithKey
  :: Ord a
  => (k -> a -> Bool) -> Dependencies k a
  -> (Dependencies k a, Dependencies k a)
partitionWithKey f (Dependencies deps rDeps) =
  let
    (tDeps, fDeps) = Map.partitionWithKey f deps
    (tRDeps, fRDeps) = Mos.partitionWithKey (flip f) rDeps
  in
    ( Dependencies fDeps fRDeps
    , Dependencies tDeps tRDeps)

partitionKey
  :: Ord a
  => (k -> Bool) -> Dependencies k a -> (Dependencies k a, Dependencies k a)
partitionKey f = partitionWithKey (\k _ -> f k)

partition
  :: Ord a
  => (a -> Bool) -> Dependencies k a -> (Dependencies k a, Dependencies k a)
partition f = partitionWithKey (\_ a -> f a)

{-# LANGUAGE
    DeriveTraversable
  , DeriveFoldable
  , DeriveFunctor
#-}

module Clapi.Types.AssocList
  ( AssocList, unAssocList, mkAssocList, unsafeMkAssocList, null
  , singleton, fromKeys, fromList, fromMap, pickFromMap
  , toMap, fromZip
  , cons, lookup, insert, setDefault, delete
  , keys, keys_, keysSet, values
  , partitionWithKey
  , fmapWithKey, mapKeys, filterWithKey, foldlWithKey,  filterKey
  , restrictKeys
  , alterF, alter, adjust
  ) where

import Prelude hiding (fail, filter, null, lookup)
import Control.Monad.Fail (MonadFail(..))
import qualified Data.Foldable as Foldable
import Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set

import Clapi.Types.UniqList (UniqList, unUniqList, unsafeMkUniqList)
import Clapi.Util (ensureUnique, strictZip, fmtStrictZipError, Filterable(..))

newtype AssocList a b
  = AssocList {unAssocList :: [(a, b)]}
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

null :: AssocList a b -> Bool
null (AssocList l) = List.null l

mkAssocList :: (MonadFail m, Ord a, Show a) => [(a, b)] -> m (AssocList a b)
mkAssocList abPairs = ensureUnique "keys" (fmap fst abPairs) >> return (AssocList abPairs)

unsafeMkAssocList :: [(a, b)] -> AssocList a b
unsafeMkAssocList = AssocList

instance Eq a => Semigroup (AssocList a b) where
  al1 <> AssocList l2 = Foldable.foldl' (\acc (a, b) -> insert a b acc) al1 l2
instance Eq a => Monoid (AssocList a b) where
  mempty = AssocList []
  mappend = (<>)

singleton :: a -> b -> AssocList a b
singleton a b = AssocList [(a, b)]

fromKeys :: (a -> b) -> UniqList a -> AssocList a b
fromKeys f as = AssocList $ (\a -> (a, f a)) <$> unUniqList as

-- | Like `Map.fromList`, in that it doesn't fail but takes the final value for
--   any duplicated key. Use `mkAssocList` to check for uniqueness.
fromList :: Eq a => [(a, b)] -> AssocList a b
fromList = foldl (\acc (k, a) -> insert k a acc) mempty

fromMap :: Map a b -> AssocList a b
fromMap = AssocList . Map.toList

pickFromMap :: Ord a => Map a b -> UniqList a -> AssocList a b
pickFromMap m ul = AssocList $ catMaybes $
  (\a -> (a,) <$> Map.lookup a m) <$> unUniqList ul

toMap :: Ord a => AssocList a b -> Map a b
toMap = Map.fromList . unAssocList

fromZip :: (MonadFail m, Ord a, Show a) => [a] -> [b] -> m (AssocList a b)
fromZip as bs = fmtStrictZipError "keys" "values" (strictZip as bs) >>= mkAssocList

cons
  :: (MonadFail m, Ord a, Show a)
  => a -> b -> AssocList a b -> m (AssocList a b)
cons a b = mkAssocList . ((a, b) :) . unAssocList

lookup :: (MonadFail m, Eq a, Show a) => a -> AssocList a b -> m b
lookup a l = maybe (fail $ "Missing " ++ show a) return $ List.lookup a $ unAssocList l

insert :: Eq k => k -> a -> AssocList k a -> AssocList k a
insert k a = alter (const $ Just a) k

setDefault :: Eq k => k -> a -> AssocList k a -> AssocList k a
setDefault k a = alter (Just . maybe a id) k

delete :: Eq k => k -> AssocList k a -> AssocList k a
delete k = alter (const Nothing) k

keys :: AssocList a b -> UniqList a
keys = unsafeMkUniqList . fmap fst . unAssocList

keys_ :: AssocList a b -> [a]
keys_ = unUniqList . keys

keysSet :: Ord a => AssocList a b -> Set a
keysSet = Set.fromList . fmap fst . unAssocList

values :: AssocList a b -> [b]
values = fmap snd . unAssocList

fmapWithKey :: (k -> b -> c) -> AssocList k b -> AssocList k c
fmapWithKey f (AssocList kbs) = AssocList $ (\(k, b) -> (k, f k b)) <$> kbs

mapKeys
  :: (Ord k1, Show k1, MonadFail m)
  => (k0 -> k1) -> AssocList k0 b -> m (AssocList k1 b)
mapKeys f = mkAssocList . fmap (\(a, b) -> (f a, b)) . unAssocList

foldlWithKey :: (acc -> k -> b -> acc) -> acc -> AssocList k b -> acc
foldlWithKey f acc = foldl (\acc' (k, b) -> f acc' k b) acc . unAssocList

partitionWithKey
  :: (k -> v -> Bool) -> AssocList k v -> (AssocList k v, AssocList k v)
partitionWithKey f al =
  let (ys, ns) = List.partition (uncurry f) $ unAssocList al in
    (AssocList ys, AssocList ns)

filterWithKey :: (k -> b -> Bool) -> AssocList k b -> AssocList k b
filterWithKey f = AssocList . filter (uncurry f) . unAssocList

filterKey :: (k -> Bool) -> AssocList k b -> AssocList k b
filterKey f = filterWithKey $ \k _ -> f k

restrictKeys :: Ord k => AssocList k a -> Set k -> AssocList k a
restrictKeys al s = filterWithKey (\k _ -> k `Set.member` s) al

alterF
  :: (Eq k, Functor f) => (Maybe b -> f (Maybe b)) -> k -> AssocList k b
  -> f (AssocList k b)
alterF f k (AssocList theKbs) = AssocList <$> alterF' theKbs
  where
    alterF' [] = Foldable.toList . fmap (k,) <$> f Nothing
    alterF' (p@(k', b):kbs)
      | k' == k = maybe kbs (\b' -> (k, b') : kbs) <$> f (Just b)
      | otherwise = (p:) <$> alterF' kbs

alter :: Eq k => (Maybe b -> Maybe b) -> k -> AssocList k b -> AssocList k b
alter f k = runIdentity . alterF (Identity . f) k

adjust :: Eq k => (b -> b) -> k -> AssocList k b -> AssocList k b
adjust f k = alter (fmap f) k


instance Filterable (AssocList k) where
  filter f = filterWithKey (const f)

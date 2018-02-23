{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}

module Clapi.Types.AssocList
  ( AssocList, unAssocList, mkAssocList, unsafeMkAssocList
  , alEmpty, alSingleton, alFromKeys, alFromList, alFromMap, alPickFromMap
  , alToMap, alFromZip
  , alCons, alLookup, alInsert, alSetDefault, alDelete
  , alKeys, alKeysSet, alValues
  , alFmapWithKey, alMapKeys, alFilterWithKey, alFoldlWithKey,  alFilterKey
  , alAlterF, alAlter, alAdjust
  ) where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import qualified Data.Foldable as Foldable
import Data.Functor.Identity (Identity(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set

import Clapi.Types.UniqList (UniqList, unUniqList, unsafeMkUniqList)
import Clapi.Util (ensureUnique, strictZip, fmtStrictZipError)

newtype AssocList a b
  = AssocList {unAssocList :: [(a, b)]}
  deriving (Show, Eq, Functor, Foldable, Traversable)

mkAssocList :: (MonadFail m, Ord a, Show a) => [(a, b)] -> m (AssocList a b)
mkAssocList abPairs = ensureUnique "keys" (fmap fst abPairs) >> return (AssocList abPairs)

unsafeMkAssocList :: [(a, b)] -> AssocList a b
unsafeMkAssocList = AssocList

alEmpty :: AssocList a b
alEmpty = AssocList []

alSingleton :: a -> b -> AssocList a b
alSingleton a b = AssocList [(a, b)]

alFromKeys :: (a -> b) -> UniqList a -> AssocList a b
alFromKeys f as = AssocList $ (\a -> (a, f a)) <$> unUniqList as

-- | Like `Map.fromList`, in that it doesn't fail but takes the final value for
--   any duplicated key. Use `mkAssocList` to check for uniqueness.
alFromList :: Eq a => [(a, b)] -> AssocList a b
alFromList = foldl (\acc (k, a) -> alInsert k a acc) alEmpty

alFromMap :: Map a b -> AssocList a b
alFromMap = AssocList . Map.toList

alPickFromMap :: Ord a => Map a b -> UniqList a -> AssocList a b
alPickFromMap m ul = AssocList $ catMaybes $
  (\a -> (a,) <$> Map.lookup a m) <$> unUniqList ul

alToMap :: Ord a => AssocList a b -> Map a b
alToMap = Map.fromList . unAssocList

alFromZip :: (MonadFail m, Ord a, Show a) => [a] -> [b] -> m (AssocList a b)
alFromZip as bs = fmtStrictZipError "keys" "values" (strictZip as bs) >>= mkAssocList

alCons
  :: (MonadFail m, Ord a, Show a)
  => a -> b -> AssocList a b -> m (AssocList a b)
alCons a b = mkAssocList . ((a, b) :) . unAssocList

alLookup :: (MonadFail m, Eq a, Show a) => a -> AssocList a b -> m b
alLookup a l = maybe (fail $ "Missing " ++ show a) return $ lookup a $ unAssocList l

alInsert :: Eq k => k -> a -> AssocList k a -> AssocList k a
alInsert k a = alAlter (const $ Just a) k

alSetDefault :: Eq k => k -> a -> AssocList k a -> AssocList k a
alSetDefault k a = alAlter (Just . maybe a id) k

alDelete :: Eq k => k -> AssocList k a -> AssocList k a
alDelete k = alAlter (const Nothing) k

alKeys :: AssocList a b -> UniqList a
alKeys = unsafeMkUniqList . fmap fst . unAssocList

alKeysSet :: Ord a => AssocList a b -> Set a
alKeysSet = Set.fromList . fmap fst . unAssocList

alValues :: AssocList a b -> [b]
alValues = fmap snd . unAssocList

alFmapWithKey :: (k -> b -> c) -> AssocList k b -> AssocList k c
alFmapWithKey f (AssocList kbs) = AssocList $ (\(k, b) -> (k, f k b)) <$> kbs

alMapKeys
  :: (Ord k1, Show k1, MonadFail m)
  => (k0 -> k1) -> AssocList k0 b -> m (AssocList k1 b)
alMapKeys f = mkAssocList . fmap (\(a, b) -> (f a, b)) . unAssocList

alFoldlWithKey :: (acc -> k -> b -> acc) -> acc -> AssocList k b -> acc
alFoldlWithKey f acc = foldl (\acc' (k, b) -> f acc' k b) acc . unAssocList

alFilterWithKey :: (k -> b -> Bool) -> AssocList k b -> AssocList k b
alFilterWithKey f = AssocList . filter (uncurry f) . unAssocList

alFilterKey :: (k -> Bool) -> AssocList k b -> AssocList k b
alFilterKey f = alFilterWithKey $ \k _ -> f k

alAlterF
  :: (Eq k, Functor f) => (Maybe b -> f (Maybe b)) -> k -> AssocList k b
  -> f (AssocList k b)
alAlterF f k (AssocList theKbs) = AssocList <$> alterF theKbs
  where
    alterF [] = Foldable.toList . fmap (k,) <$> f Nothing
    alterF (p@(k', b):kbs)
      | k' == k = maybe kbs (\b' -> (k, b') : kbs) <$> f (Just b)
      | otherwise = (p:) <$> alterF kbs

alAlter :: Eq k => (Maybe b -> Maybe b) -> k -> AssocList k b -> AssocList k b
alAlter f k = runIdentity . alAlterF (Identity . f) k

alAdjust :: Eq k => (b -> b) -> k -> AssocList k b -> AssocList k b
alAdjust f k = alAlter (fmap f) k
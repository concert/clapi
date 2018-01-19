{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TupleSections #-}

module Clapi.Types.AssocList
  ( mkAssocList, unAssocList, AssocList, alSingleton, alFromKeys, alFromMap
  , alFromZip, alLookup, alKeys, alValues, alFmapWithKey, alAlterF, alAlter
  , alAdjust
  ) where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import qualified Data.Foldable as Foldable
import Data.Functor.Identity (Identity(..))
import Data.Map (Map)
import qualified Data.Map as Map

import Clapi.Types.UniqList (UniqList, unUniqList)
import Clapi.Util (ensureUnique, strictZip, fmtStrictZipError)

newtype AssocList a b
  = AssocList {unAssocList :: [(a, b)]} deriving (Show, Eq, Functor, Foldable)

mkAssocList :: (MonadFail m, Ord a, Show a) => [(a, b)] -> m (AssocList a b)
mkAssocList abPairs = ensureUnique "keys" (fmap fst abPairs) >> return (AssocList abPairs)

alSingleton :: a -> b -> AssocList a b
alSingleton a b = AssocList [(a, b)]

alFromKeys :: b -> UniqList a -> AssocList a b
alFromKeys b as = AssocList $ zip (unUniqList as) $ repeat b

alFromMap :: Map a b -> AssocList a b
alFromMap = AssocList . Map.toList

alFromZip :: (MonadFail m, Ord a, Show a) => [a] -> [b] -> m (AssocList a b)
alFromZip as bs = fmtStrictZipError "keys" "values" (strictZip as bs) >>= mkAssocList

alLookup :: (MonadFail m, Eq a, Show a) => a -> AssocList a b -> m b
alLookup a l = maybe (fail $ "Missing " ++ show a) return $ lookup a $ unAssocList l

alKeys :: AssocList a b -> [a]
alKeys = fmap fst . unAssocList

alValues :: AssocList a b -> [b]
alValues = fmap snd . unAssocList

alFmapWithKey :: (k -> b -> c) -> AssocList k b -> AssocList k c
alFmapWithKey f (AssocList kbs) = AssocList $ (\(k, b) -> (k, f k b)) <$> kbs

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

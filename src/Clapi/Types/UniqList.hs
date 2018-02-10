{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE DeriveFoldable #-}

module Clapi.Types.UniqList
  ( UniqList, unUniqList, unsafeMkUniqList
  , mkUniqList, ulFromSet, ulEmpty, ulSingle
  , ulDelete, ulInsert
  ) where

import Control.Monad.Fail (MonadFail)
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set

import Clapi.Util (ensureUnique)

newtype UniqList a
  = UniqList {unUniqList :: [a]} deriving (Show, Eq, Ord, Foldable)

mkUniqList :: (Ord a, Show a, MonadFail m) => [a] -> m (UniqList a)
mkUniqList as = UniqList <$> ensureUnique "items" as

unsafeMkUniqList :: [a] -> UniqList a
unsafeMkUniqList = UniqList

ulFromSet :: Ord a => Set a -> UniqList a
ulFromSet = UniqList . Set.toList

ulEmpty :: UniqList a
ulEmpty = UniqList []

ulSingle :: a -> UniqList a
ulSingle a = UniqList [a]

ulDelete :: Eq a => a -> UniqList a -> UniqList a
ulDelete a (UniqList as) = UniqList $ List.delete a as

ulInsert :: Eq a => a -> UniqList a -> UniqList a
ulInsert a' = UniqList . inner . unUniqList
  where
    inner [] = [a']
    inner l@(a:as) | a' == a = l
                   | otherwise = a : inner as

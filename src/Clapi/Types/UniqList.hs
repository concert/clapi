{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE DeriveFunctor #-}

module Clapi.Types.UniqList
  ( UniqList, unUniqList
  , mkUniqList, ulFromSet, ulEmpty, ulSingle
  , ulDelete
  ) where

import Control.Monad.Fail (MonadFail)
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set

import Clapi.Util (ensureUnique)

newtype UniqList a
  = UniqList {unUniqList :: [a]} deriving (Show, Eq, Ord, Functor)

mkUniqList :: (Ord a, Show a, MonadFail m) => [a] -> m (UniqList a)
mkUniqList as = UniqList <$> ensureUnique "items" as

ulFromSet :: Ord a => Set a -> UniqList a
ulFromSet = UniqList . Set.toList

ulEmpty :: UniqList a
ulEmpty = UniqList []

ulSingle :: a -> UniqList a
ulSingle a = UniqList [a]

instance Foldable UniqList where
  foldMap f = foldMap f . unUniqList

ulDelete :: Eq a => a -> UniqList a -> UniqList a
ulDelete a (UniqList as) = UniqList $ List.delete a as

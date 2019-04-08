{-# LANGUAGE DeriveFoldable #-}

module Clapi.Types.UniqList
  ( UniqList, unUniqList, unsafeMkUniqList
  , mkUniqList, ulFromSet, ulEmpty, ulSingle
  , ulDelete, ulInsert, ulPresentAfter', ulPresentAfter
  ) where

import Prelude hiding (fail)

import Control.Monad (unless)
import Control.Monad.Fail (MonadFail(..))
import Data.Foldable (foldl')
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

ulPresentAfter
  :: (MonadFail m, Eq a) => a -> Maybe a -> UniqList a -> m (UniqList a)
ulPresentAfter item ref ul = case ref of
    Nothing -> return $ UniqList $ item : List.delete item (unUniqList ul)
    Just r -> do
      -- FIXME: Would be good to remove this failure mode and deal with it
      -- higher up in the code:
      unless (r `elem` ul) $ fail "Preceeding element not found for move"
      return $ ulPresentAfter' item r ul

-- NB: if the item's "after this" target is not in the list, we will just drop
-- the item:
ulPresentAfter' :: Eq a => a -> a -> UniqList a -> UniqList a
ulPresentAfter' item ref = UniqList . go . unUniqList
  where
    go [] = []
    go (a:as)
      | a == item = go as
      | a == ref = a : item : go as
      | otherwise = a : go as

instance Eq a => Semigroup (UniqList a) where
  ul <> UniqList l = foldl' (flip ulInsert) ul l

instance Eq a => Monoid (UniqList a) where
  mempty = ulEmpty

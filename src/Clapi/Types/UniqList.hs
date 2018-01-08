{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE DeriveFunctor #-}

module Clapi.Types.UniqList
  ( UniqList, unUniqList
  , mkUniqList
  ) where

import Control.Monad.Fail (MonadFail)

import Clapi.Util (ensureUnique)

newtype UniqList a = UniqList {unUniqList :: [a]} deriving (Show, Eq, Functor)

mkUniqList :: (Ord a, Show a, MonadFail m) => [a] -> m (UniqList a)
mkUniqList as = UniqList <$> ensureUnique "items" as

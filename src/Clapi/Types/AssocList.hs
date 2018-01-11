{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE DeriveFunctor #-}

module Clapi.Types.AssocList (mkAssocList, unAssocList, AssocList, alLookup) where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))

import Clapi.Util (ensureUnique)

newtype AssocList a b = AssocList {unAssocList :: [(a, b)]} deriving (Show, Eq, Functor)

mkAssocList :: (MonadFail m, Ord a, Show a) => [(a, b)] -> m (AssocList a b)
mkAssocList abPairs = ensureUnique "keys" (fmap fst abPairs) >> return (AssocList abPairs)

alLookup :: (MonadFail m, Eq a, Show a) => a -> AssocList a b -> m b
alLookup a l = maybe (fail $ "Missing " ++ show a) return $ lookup a $ unAssocList l

{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE DeriveFunctor #-}

module Clapi.Types.AssocList
  ( mkAssocList, unAssocList, AssocList, alFromMap, alFromZip, alLookup, alKeys
  , alValues ) where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Data.Map (Map)
import qualified Data.Map as Map

import Clapi.Util (ensureUnique, strictZip, fmtStrictZipError)

newtype AssocList a b = AssocList {unAssocList :: [(a, b)]} deriving (Show, Eq, Functor)

mkAssocList :: (MonadFail m, Ord a, Show a) => [(a, b)] -> m (AssocList a b)
mkAssocList abPairs = ensureUnique "keys" (fmap fst abPairs) >> return (AssocList abPairs)

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

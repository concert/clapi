{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Clapi.Types
  ( CanFail, safeToEnum
  , module X
  ) where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail, fail)

import Clapi.Types.Base as X
import Clapi.Types.UniqList as X
import Clapi.Types.Wire as X
import Clapi.Types.Messages as X
import Clapi.Types.Tree as X

type CanFail a = Either String a

instance MonadFail (Either String) where
    fail s = Left s

-- http://stackoverflow.com/questions/2743858/safe-and-polymorphic-toenum
safeToEnum :: (MonadFail m, Enum a, Bounded a) => Int -> m a
safeToEnum i =
  let
    r = toEnum i
    theMax = maxBound `asTypeOf` r
    theMin = minBound `asTypeOf` r
  in if fromEnum theMin <= i && i <= fromEnum theMax
  then return r
  else fail "enum value out of range"

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Clapi.Types
  ( CanFail
  , module X
  ) where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail, fail)

import Clapi.Types.AssocList as X
import Clapi.Types.Base as X
import Clapi.Types.Definitions as X
import Clapi.Types.Digests as X
import Clapi.Types.Nat as X
import Clapi.Types.Path as X
import Clapi.Types.UniqList as X
import Clapi.Types.Wire as X
import Clapi.Types.Symbol as X
import Clapi.Types.SymbolList as X
import Clapi.Types.Tree as X

type CanFail a = Either String a

instance MonadFail (Either String) where
    fail s = Left s

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    FlexibleInstances
  , GeneralizedNewtypeDeriving
  , StandaloneDeriving
  , TypeSynonymInstances
#-}

module Clapi.Serialisation.Path where

import Clapi.Serialisation.Base (Encodable(..))
import qualified Clapi.Types.Path as Path

instance Encodable Path.Seg where
  builder = builder . Path.unSeg
  parser = parser >>= Path.mkSeg

deriving instance Encodable Path.Placeholder

instance Encodable Path.Path where
  builder = builder . Path.toText Path.unSeg
  parser = parser >>= Path.fromText Path.segP

deriving instance Encodable Path.Namespace

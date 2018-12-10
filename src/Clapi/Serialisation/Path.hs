{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    FlexibleInstances
  , GeneralizedNewtypeDeriving
  , StandaloneDeriving
  , TypeSynonymInstances
#-}

module Clapi.Serialisation.Path where

import Clapi.Serialisation.Base (Encodable(..), Decodable(..))
import Clapi.Types.Path (Path, Seg, Placeholder, Namespace)
import qualified Clapi.Types.Path as Path

instance Encodable Seg where
  builder = builder . Path.unSeg
instance Decodable Seg where
  parser = parser >>= Path.mkSeg

deriving instance Encodable Placeholder
deriving instance Decodable Placeholder

instance Encodable Path where
  builder = builder . Path.toText Path.unSeg
instance Decodable Path where
  parser = parser >>= Path.fromText Path.segP

deriving instance Encodable Namespace
deriving instance Decodable Namespace

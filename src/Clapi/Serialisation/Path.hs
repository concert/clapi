{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    FlexibleInstances
#-}

module Clapi.Serialisation.Path where

import Clapi.Serialisation.Base (Encodable(..), Decodable(..))
import Clapi.Types.Path (Path, Name)
import qualified Clapi.Types.Path as Path

instance Encodable (Name nr) where
  builder = builder . Path.unName
instance Decodable (Name nr) where
  parser = parser >>= Path.mkName

instance Encodable Path where
  builder = builder . Path.toText Path.unName
instance Decodable Path where
  parser = parser >>= Path.fromText Path.nameP


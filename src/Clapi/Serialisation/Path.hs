{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    FlexibleInstances
#-}

module Clapi.Serialisation.Path where

import Clapi.Serialisation.Base (Encodable(..), Decodable(..))
import Clapi.Types.Name (nameP, unName)
import Clapi.Types.Path (Path)
import qualified Clapi.Types.Path as Path

instance Encodable Path where
  builder = builder . Path.toText unName
instance Decodable Path where
  parser = parser >>= Path.fromText nameP

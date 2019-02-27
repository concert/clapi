{-# OPTIONS_GHC -Wno-orphans #-}

module Clapi.Serialisation.Name where

import Clapi.Serialisation.Base (Encodable(..), Decodable(..))
import Clapi.Types.Name (Name, mkName, unName)

instance Encodable (Name nr) where
  builder = builder . unName
instance Decodable (Name nr) where
  parser = parser >>= mkName

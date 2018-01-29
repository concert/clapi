{-# OPTIONS_GHC -Wall -Wno-orphans #-}

module Clapi.Serialisation.Definitions where

import Clapi.Serialisation.Base (Encodable(..))
import Clapi.Types.Definitions
  ( Liberty(..)
  , TupleDefinition(..), StructDefinition(..), ArrayDefinition(..)
  , Definition(..))

instance Encodable Liberty where
  builder = undefined
  parser = undefined

instance Encodable TupleDefinition where
  builder = undefined
  parser = undefined

instance Encodable StructDefinition where
  builder = undefined
  parser = undefined

instance Encodable ArrayDefinition where
  builder = undefined
  parser = undefined

instance Encodable Definition where
  builder = undefined
  parser = undefined

{-# OPTIONS_GHC -Wall -Wno-orphans #-}

module Clapi.Serialisation.Path where

import Clapi.Serialisation.Base (Encodable(..))
import qualified Clapi.Types.Path as Path

instance Encodable Path.Seg where
  builder = builder . Path.unSeg
  parser = parser >>= Path.mkSeg

instance Encodable Path.Path where
  builder = builder . Path.toText
  parser = parser >>= Path.fromText

instance Encodable Path.TypeName where
  builder = builder . Path.typeNameToText
  parser = parser >>= Path.typeNameFromText

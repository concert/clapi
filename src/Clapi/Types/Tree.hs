{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Clapi.Types.Tree where

import Data.Int
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word

import Clapi.Path (Path, Seg, mkSeg)
import Clapi.Util (uncamel)


data Bounds a
  = Bounds {boundsMin :: Maybe a, boundsMax :: Maybe a}
  deriving (Show, Eq, Ord)

unbounded :: Bounds a
unbounded = Bounds Nothing Nothing


data TreeConcreteTypeName
  = TcnTime
  | TcnEnum
  | TcnWord32 | TcnWord64
  | TcnInt32 | TcnInt64
  | TcnFloat | TcnDouble
  | TcnString | TcnRef | TcnValidatorDesc
  deriving (Show, Eq, Ord, Enum, Bounded)

data TreeConcreteType
  = TcTime
  | TcEnum [Seg]
  | TcWord32 (Bounds Word32)
  | TcWord64 (Bounds Word64)
  | TcInt32 (Bounds Int32)
  | TcInt64 (Bounds Int64)
  | TcFloat (Bounds Float)
  | TcDouble (Bounds Double)
  | TcString Text
  | TcRef Path
  | TcValidatorDesc
  deriving (Show, Eq, Ord)

data TreeContainerTypeName
  = TcnList
  | TcnSet
  | TcnOrdSet
  deriving (Show, Eq, Ord, Enum, Bounded)

data TreeContainerType
  = TcList TreeType
  | TcSet TreeType
  | TcOrdSet TreeType
  deriving (Show, Eq, Ord)

data TreeType
  = TtConc TreeConcreteType
  | TtCont TreeContainerType
  deriving (Show, Eq, Ord)

ttEnum :: forall a. (Enum a, Bounded a, Show a) => Proxy a -> TreeType
ttEnum _ = TtConc $ TcEnum $
  fmap (fromJust . mkSeg . Text.pack . uncamel . show) [minBound :: a..]

{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Clapi.Types.Tree
  ( Bounds, bounds, unbounded, boundsMin, boundsMax
  , TreeConcreteTypeName(..), TreeConcreteType(..)
  , TreeContainerTypeName(..), TreeContainerType(..)
  , TreeType(..), TypeEnumOf(..)
  , tcEnum
  , ttTime, ttEnum, ttWord32, ttWord64, ttInt32, ttInt64, ttDouble, ttFloat
  , ttString, ttRef, ttValidatorDesc
  ) where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Data.Int
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word

import Clapi.Types.Path (Seg, mkSeg, TypeName)
import Clapi.Util (uncamel)


data Bounds a
  = Bounds (Maybe a) (Maybe a)
  deriving (Show, Eq, Ord)

boundsMin, boundsMax :: Bounds a -> Maybe a
boundsMin (Bounds m _) = m
boundsMax (Bounds _ m) = m

bounds :: (MonadFail m, Ord a) => Maybe a -> Maybe a -> m (Bounds a)
bounds m0@(Just bMin) m1@(Just bMax)
    | bMin <= bMax = return $ Bounds m0 m1
    | otherwise = fail "minBound > maxBound"
bounds m0 m1 = return $ Bounds m0 m1

unbounded :: Bounds a
unbounded = Bounds Nothing Nothing


class (Bounded b, Enum b) => TypeEnumOf a b | a -> b where
  typeEnumOf :: a -> b


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
  | TcRef TypeName
  | TcValidatorDesc
  deriving (Show, Eq, Ord)

instance TypeEnumOf TreeConcreteType TreeConcreteTypeName where
  typeEnumOf tct = case tct of
      TcTime -> TcnTime
      TcEnum _ -> TcnEnum
      TcWord32 _ -> TcnWord32
      TcWord64 _ -> TcnWord64
      TcInt32 _ -> TcnInt32
      TcInt64 _ -> TcnInt64
      TcFloat _ -> TcnFloat
      TcDouble _ -> TcnDouble
      TcString _ -> TcnString
      TcRef _ -> TcnRef
      TcValidatorDesc -> TcnValidatorDesc

tcEnum :: forall a. (Enum a, Bounded a, Show a) => Proxy a -> TreeConcreteType
tcEnum _ = TcEnum $
  fmap (fromJust . mkSeg . Text.pack . uncamel . show) [minBound :: a..]

data TreeContainerTypeName
  = TcnList
  | TcnSet
  | TcnOrdSet
  | TcnMaybe
  deriving (Show, Eq, Ord, Enum, Bounded)

data TreeContainerType
  = TcList {contTContainedType :: TreeType}
  | TcSet {contTContainedType :: TreeType}
  | TcOrdSet {contTContainedType :: TreeType}
  | TcMaybe {contTContainedType :: TreeType}
  deriving (Show, Eq, Ord)

instance TypeEnumOf TreeContainerType TreeContainerTypeName where
  typeEnumOf tct = case tct of
    TcList _ -> TcnList
    TcSet _ -> TcnSet
    TcOrdSet _ -> TcnOrdSet
    TcMaybe _ -> TcnMaybe

data TreeType
  = TtConc TreeConcreteType
  | TtCont TreeContainerType
  deriving (Show, Eq, Ord)

ttTime :: TreeType
ttTime = TtConc $ TcTime

ttEnum :: forall a. (Enum a, Bounded a, Show a) => Proxy a -> TreeType
ttEnum = TtConc . tcEnum

ttWord32 :: Bounds Word32 -> TreeType
ttWord32 = TtConc . TcWord32

ttWord64 :: Bounds Word64 -> TreeType
ttWord64 = TtConc . TcWord64

ttInt32 :: Bounds Int32 -> TreeType
ttInt32 = TtConc . TcInt32

ttInt64 :: Bounds Int64 -> TreeType
ttInt64 = TtConc . TcInt64

ttFloat :: Bounds Float -> TreeType
ttFloat = TtConc . TcFloat

ttDouble :: Bounds Double -> TreeType
ttDouble = TtConc . TcDouble

ttString :: Text -> TreeType
ttString = TtConc . TcString

ttRef :: TypeName -> TreeType
ttRef = TtConc . TcRef

ttValidatorDesc :: TreeType
ttValidatorDesc = TtConc $ TcValidatorDesc

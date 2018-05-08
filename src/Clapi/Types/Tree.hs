{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module Clapi.Types.Tree
  ( Bounds, bounds, unbounded, boundsMin, boundsMax
  , TreeConcreteTypeName(..), TreeConcreteType(..)
  , TreeContainerTypeName(..), TreeContainerType(..)
  , TreeType(..), TypeEnumOf(..)
  , tcEnum
  , ttTime, ttEnum, ttWord32, ttWord64, ttInt32, ttInt64, ttDouble, ttFloat
  , ttString, ttRef, ttValidatorDesc
  , ttList, ttSet, ttOrdSet, ttMaybe, ttPair
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

-- FIXME: might want to move these elsewhere
import Clapi.Types.Wire (Wireable)


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

ttConcMagic
  :: TreeConcreteType -> (forall (a :: *). Wireable a => Proxy a -> r) -> r
ttConcMagic tct f = case tct of
  TcWord32 _ -> f $ Proxy @Word32
  -- FIXME: etc

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
  | TcnPair
  deriving (Show, Eq, Ord, Enum, Bounded)

data TreeContainerType
  = TcList {contTContainedType :: TreeType}
  | TcSet {contTContainedType :: TreeType}
  | TcOrdSet {contTContainedType :: TreeType}
  | TcMaybe {contTContainedType :: TreeType}
  | TcPair TreeType TreeType -- FIXME: this is a pain
  deriving (Show, Eq, Ord)

ttContMagic
  :: TreeContainerType -> (forall (a :: *). Wireable a => Proxy a -> r) -> r
ttContMagic tct f = case tct of
  TcList tt -> magic tt (\pConc -> f $ proxyF (Proxy @[]) pConc)
  -- FIXME: etc...
  TcPair tt1 tt2 ->
    magic tt1 $ \pConc1 ->
      magic tt2 $ \pConc2 -> f $ proxyF3 (Proxy @(,)) pConc1 pConc2

instance TypeEnumOf TreeContainerType TreeContainerTypeName where
  typeEnumOf tct = case tct of
    TcList _ -> TcnList
    TcSet _ -> TcnSet
    TcOrdSet _ -> TcnOrdSet
    TcMaybe _ -> TcnMaybe
    TcPair _ _ -> TcnPair

data TreeType
  = TtConc TreeConcreteType
  | TtCont TreeContainerType
  deriving (Show, Eq, Ord)

magic :: TreeType -> (forall (a :: *). Wireable a => Proxy a -> r) -> r
magic tt f = case tt of
  TtConc tct -> ttConcMagic tct f
  TtCont tct -> ttContMagic tct f

proxyF :: Proxy a -> Proxy b -> Proxy (a b)
proxyF _ _ = Proxy

proxyF3 :: Proxy a -> Proxy b -> Proxy c -> Proxy (a b c)
proxyF3 p1 p2 p3 = proxyF (proxyF p1 p2) p3

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

ttList :: TreeType -> TreeType
ttList = TtCont . TcList

ttSet :: TreeType -> TreeType
ttSet = TtCont . TcSet

ttOrdSet :: TreeType -> TreeType
ttOrdSet = TtCont . TcOrdSet

ttMaybe :: TreeType -> TreeType
ttMaybe = TtCont . TcMaybe

ttPair :: TreeType -> TreeType -> TreeType
ttPair tt1 tt2 = TtCont $ TcPair tt1 tt2

{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Clapi.Types.Wire
  ( WireConcreteType(..), WireContainerType(..), WireType(..)
  , Wireable, toHaskell
  , WireValue, wireValue, wireTypeOf
  ) where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Data.Int
import Data.Proxy
import Data.Text (Text)
import Data.Word
import Unsafe.Coerce

import Clapi.Types.Base (Time)

data WireConcreteType
  = WcTime
  | WcWord8 | WcWord32 | WcWord64
  | WcInt32 | WcInt64
  | WcFloat | WcDouble
  | WcString
  deriving (Show, Eq, Ord, Enum, Bounded)

data WireContainerType
  = WcList
  deriving (Show, Eq, Ord, Enum, Bounded)

data WireType
  = WtConc WireConcreteType
  | WtCont WireContainerType WireType
  deriving (Show, Eq, Ord)

getTyInfo :: WireType -> ([WireContainerType], WireConcreteType)
getTyInfo wt = let (l, t) = inner wt in (reverse l, t)
  where
    inner (WtConc concT) = ([], concT)
    inner (WtCont contT wt') =
        let (l, concT) = getTyInfo wt' in (contT : l, concT)

class (Show a, Eq a, Ord a) => Wireable a where
  toWireType :: proxy a -> WireType
  toHaskell :: MonadFail m => WireValue -> m a
  toHaskell (WireValue wt v) =
    let expectedType = toWireType (Proxy :: Proxy a) in
    if wt == expectedType
    then return $ unsafeCoerce v
    else fail $ "Type mismatch: tried to unpack value of "
         ++ show wt ++ " as " ++ show expectedType

instance Wireable Time where
  toWireType _ = WtConc WcTime

instance Wireable Word8 where
  toWireType _ = WtConc WcWord8
instance Wireable Word32 where
  toWireType _ = WtConc WcWord32
instance Wireable Word64 where
  toWireType _ = WtConc WcWord64

instance Wireable Int32 where
  toWireType _ = WtConc WcInt32
instance Wireable Int64 where
  toWireType _ = WtConc WcInt64

instance Wireable Float where
  toWireType _ = WtConc WcFloat
instance Wireable Double where
  toWireType _ = WtConc WcDouble

instance Wireable Text where
  toWireType _ = WtConc WcString

instance Wireable a => Wireable [a] where
  toWireType _ = WtCont WcList $ toWireType (Proxy :: Proxy a)


data WireValue = forall a. Wireable a => WireValue WireType a

wireValue :: forall a. Wireable a => a -> WireValue
wireValue a = WireValue (toWireType (Proxy :: Proxy a)) a

wireTypeOf :: WireValue -> WireType
wireTypeOf (WireValue wt _) = wt


instance Show WireValue where
  show = wireApply show

instance Eq WireValue where
  (==) = binaryWireApply (==) (==)

instance Ord WireValue where
  compare = binaryWireApply compare compare


wireApply :: forall r. (forall x. Wireable x => x -> r) -> WireValue -> r
wireApply f (WireValue wt v) =
  let (tyL, concT) = getTyInfo wt in case concT of
    WcTime -> nestWire (Proxy :: Proxy Time) tyL
    WcWord8 -> nestWire (Proxy :: Proxy Word8) tyL
    WcWord32 -> nestWire (Proxy :: Proxy Word32) tyL
    WcWord64 -> nestWire (Proxy :: Proxy Word64) tyL
    WcInt32 -> nestWire (Proxy :: Proxy Int32) tyL
    WcInt64 -> nestWire (Proxy :: Proxy Int64) tyL
    WcFloat -> nestWire (Proxy :: Proxy Float) tyL
    WcDouble -> nestWire (Proxy :: Proxy Double) tyL
    WcString -> nestWire (Proxy :: Proxy Text) tyL
  where
    nestWire
      :: forall a. Wireable a
      => Proxy a -> [WireContainerType] -> r
    nestWire _ [] = f @a $ unsafeCoerce v
    nestWire _ (contT:contTs) = case contT of
      WcList -> nestWire (Proxy :: Proxy [a]) contTs

binaryWireApply
  :: forall r.
     (WireType -> WireType -> r)
  -> (forall x. Wireable x => x -> x -> r)
  -> WireValue -> WireValue -> r
binaryWireApply onTyMismatch f (WireValue wt0 v0) (WireValue wt1 v1)
  | wt0 == wt1 = let (tyL, concT) = getTyInfo wt0 in case concT of
      WcTime -> nestWire (Proxy :: Proxy Time) tyL
      WcWord8 -> nestWire (Proxy :: Proxy Word8) tyL
      WcWord32 -> nestWire (Proxy :: Proxy Word32) tyL
      WcWord64 -> nestWire (Proxy :: Proxy Word64) tyL
      WcInt32 -> nestWire (Proxy :: Proxy Int32) tyL
      WcInt64 -> nestWire (Proxy :: Proxy Int64) tyL
      WcFloat -> nestWire (Proxy :: Proxy Float) tyL
      WcDouble -> nestWire (Proxy :: Proxy Double) tyL
      WcString -> nestWire (Proxy :: Proxy Text) tyL
  | otherwise = onTyMismatch wt0 wt1
  where
    nestWire
      :: forall a. Wireable a
      => Proxy a -> [WireContainerType] -> r
    nestWire _ [] = f @a (unsafeCoerce v0) (unsafeCoerce v1)
    nestWire _ (contT:contTs) = case contT of
      WcList -> nestWire (Proxy :: Proxy [a]) contTs

{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module Clapi.Types.Wire
  ( Wireable -- , toHaskell
  , WireValue(..)
  , apply
  ) where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Data.Int
import Data.Text (Text)
import Data.Word
import Data.Typeable

import Clapi.Types.Base (Time)

cast' :: forall a b m. (Typeable a, Typeable b, MonadFail m) => a -> m b
cast' a =
  let
    die = fail $ "Type mismatch: tried to cast value of type "
      ++ show (typeOf a) ++ " to " ++ show (typeRep (Proxy :: Proxy b))
  in
    maybe die return $ cast a

class (Typeable a, Show a, Eq a, Ord a) => Wireable a
instance Wireable Time where
instance Wireable Word8
instance Wireable Word32
instance Wireable Word64
instance Wireable Int32
instance Wireable Int64
instance Wireable Float
instance Wireable Double
instance Wireable Text
instance Wireable a => Wireable [a]

data WireValue = forall a. Wireable a => WireValue a

deriving instance Show WireValue

instance Eq WireValue where
  (WireValue x) == (WireValue y) = maybe False (x ==) $ cast y

instance Ord WireValue where
  compare (WireValue x) (WireValue y) =
    maybe (compare (typeOf x) (typeOf y)) (compare x) $ cast y

castWireValue :: (MonadFail m, Wireable a) => WireValue -> m a
castWireValue (WireValue x) = cast' x

apply
  :: (Wireable a, Wireable b, MonadFail m)
  => (a -> b) -> WireValue -> m WireValue
apply f = fmap (WireValue . f) . castWireValue

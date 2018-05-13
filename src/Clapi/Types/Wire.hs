{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE
    ExistentialQuantification
  , ScopedTypeVariables
  , StandaloneDeriving
#-}

module Clapi.Types.Wire
  ( Wireable
  , WireValue(..), castWireValue
  , (<|$|>), (<|*|>)
  , cast'
  ) where

import Prelude hiding (fail)

import Control.Monad.Fail (MonadFail(..))
import Data.Int
import Data.Text (Text)
import Data.Word
import Data.Typeable

import Clapi.Serialisation.Base (Encodable)
import Clapi.Types.Base (Time(..))

cast' :: forall b a m. (Typeable a, Typeable b, MonadFail m) => a -> m b
cast' a =
  let
    die = fail $ "Type mismatch: tried to cast value of type "
      ++ show (typeOf a) ++ " to " ++ show (typeRep (Proxy :: Proxy b))
  in
    maybe die return $ cast a


class (Typeable a, Show a, Eq a, Ord a, Encodable a) => Wireable a
instance Wireable Time
instance Wireable Word8
instance Wireable Word32
instance Wireable Word64
instance Wireable Int32
instance Wireable Int64
instance Wireable Float
instance Wireable Double
instance Wireable Text
instance Wireable a => Wireable [a]
instance Wireable a => Wireable (Maybe a)
instance (Wireable a, Wireable b) => Wireable (a, b)

data WireValue = forall a. Wireable a => WireValue a

deriving instance Show WireValue

instance Eq WireValue where
  (WireValue x) == (WireValue y) = maybe False (x ==) $ cast y

instance Ord WireValue where
  compare (WireValue x) (WireValue y) =
    maybe (compare (typeOf x) (typeOf y)) (compare x) $ cast y

castWireValue :: forall a m. (MonadFail m, Wireable a) => WireValue -> m a
castWireValue (WireValue x) = cast' x

(<|$|>) :: (Wireable a, MonadFail m) => (a -> b) -> WireValue -> m b
f <|$|> wv = f <$> castWireValue wv

(<|*|>) :: (Wireable a, MonadFail m) => m (a -> b) -> WireValue -> m b
mf <|*|> wv = mf <*> castWireValue wv

{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE
    AllowAmbiguousTypes
  , ExistentialQuantification
  , PolyKinds
  , Rank2Types
  , ScopedTypeVariables
  , StandaloneDeriving
  , TemplateHaskell
  , TypeApplications
#-}

module Clapi.Types.Wire
  ( Wireable
  , WireValue(..), castWireValue
  , (<|$|>), (<|*|>)
  , cast'
  , WireType(..), wireValueWireType, withWtProxy
  ) where

import Prelude hiding (fail)

import Control.Monad.Fail (MonadFail(..))
import Data.Int
import Data.Text (Text)
import Data.Word
import Data.Typeable

import Clapi.Serialisation.Base (Encodable)
import Clapi.Types.Base (Time(..))
import Clapi.Types.WireTH (mkWithWtProxy)
import Clapi.Util (proxyF, proxyF3)

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


data WireType
  = WtTime
  | WtWord8 | WtWord32 | WtWord64
  | WtInt32 | WtInt64
  | WtFloat | WtDouble
  | WtString
  | WtList WireType
  | WtMaybe WireType
  | WtPair WireType WireType
  deriving (Show, Eq, Ord)

withWtProxy = $(mkWithWtProxy [''Wireable])

wireValueWireType :: WireValue -> WireType
wireValueWireType (WireValue a) = go $ typeOf a
  where
    go :: TypeRep -> WireType
    go tr | tc == f @Time = WtTime
          | tc == f @Word8 = WtWord8
          | tc == f @Word32 = WtWord32
          | tc == f @Word64 = WtWord64
          | tc == f @Int32 = WtInt32
          | tc == f @Int64 = WtInt64
          | tc == f @Float = WtFloat
          | tc == f @Double = WtDouble
          | tc == f @Text = WtString
          | tc == f @[] = WtList $ go $ head $ typeRepArgs tr
          | tc == f @Maybe = WtMaybe $ go $ head $ typeRepArgs tr
          | tc == f @(,) =
            twoHead (\tr1 tr2 -> WtPair (go tr1) (go tr2)) $ typeRepArgs tr
          | otherwise = error $ show tc
      where tc = typeRepTyCon tr
    -- NB: this needs AllowAmbiguousTypes
    f :: forall a. Typeable a => TyCon
    f = typeRepTyCon $ typeRep $ Proxy @a
    twoHead :: (a -> a -> r) -> [a] -> r
    twoHead g (a1:a2:_) = g a1 a2
    twoHead _ _ = error "Must give two-arg typerep"

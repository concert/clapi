{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Clapi.Types.Wire where
  -- ( Wireable
  -- , WireValue(..)
  -- , apply
  -- ) where

import Prelude hiding (fail)

import Control.Applicative ((<|>))
import Data.Maybe

-- For building:
import Blaze.ByteString.Builder
  ( Builder, fromWord8, fromWord8s, fromWord16be, fromWord64be, fromWord32be
  , fromInt32be, fromInt64be)
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Data.ByteString.Builder (floatBE, doubleBE)
import Data.Monoid

-- For parsing:
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.ByteString (Parser, anyWord8, count)
import Data.Attoparsec.Binary (anyWord16be, anyWord32be, anyWord64be)
import Data.Binary.IEEE754 (wordToFloat, wordToDouble)
import Data.Text.Encoding (decodeUtf8With)

-- Wire type stuff:
import Control.Monad.Fail (MonadFail(..))
import Data.Int
import Data.Text (Text)
import Data.Word
import Data.Typeable

import Clapi.Types.Base (Time(..))
import Clapi.Types.ByteTagQ (btq)
import Clapi.Util (bound, ensureUnique)

-- Serialisation stuff that should live somewhere else:

class Encodable a where
  encode :: MonadFail m => a -> m Builder
  decode :: Parser a

instance Encodable Time where
  encode (Time w64 w32) = return $ fromWord64be w64 <> fromWord32be w32
  decode = Time <$> anyWord64be <*> anyWord32be

instance Encodable Word8 where
  encode = return . fromWord8
  decode = anyWord8
instance Encodable Word32 where
  encode = return . fromWord32be
  decode = anyWord32be
instance Encodable Word64 where
  encode = return . fromWord64be
  decode = anyWord64be

instance Encodable Int32 where
  encode = return . fromInt32be
  decode = fromIntegral <$> anyWord32be
instance Encodable Int64 where
  encode = return . fromInt64be
  decode = fromIntegral <$> anyWord64be

instance Encodable Float where
  encode = return . floatBE
  decode = wordToFloat <$> anyWord32be
instance Encodable Double where
  encode = return . doubleBE
  decode = wordToDouble <$> anyWord64be

instance Encodable Text where
  encode t = return $ fromText t <> fromWord8 0
  decode = DAB.takeWhile (/= 0) <* anyWord8 >>= return . decodeUtf8With onError
    where
      onError :: String -> Maybe Word8 -> Maybe Char
      onError _ Nothing = Nothing  -- Unexpected end of Input
      onError _ _ = Just '?'  -- Undecodable

instance Encodable a => Encodable [a] where
  encode = encodeList encode
  decode = decodeList decode

encodeList :: MonadFail m => (a -> m Builder) -> [a] -> m Builder
encodeList enc as = do
    len <- bound (length as)
    builders <- mapM enc as
    return $ fromWord16be len <> mconcat builders

decodeList :: Parser a -> Parser [a]
decodeList dec = do
    len <- anyWord16be
    count (fromIntegral len) dec



-- Wire type stuff, that depends on serialisation:

cast' :: forall a b m. (Typeable a, Typeable b, MonadFail m) => a -> m b
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

data WireValue = forall a. Wireable a => WireValue a

deriving instance Show WireValue

instance Eq WireValue where
  (WireValue x) == (WireValue y) = maybe False (x ==) $ cast y

instance Ord WireValue where
  compare (WireValue x) (WireValue y) =
    maybe (compare (typeOf x) (typeOf y)) (compare x) $ cast y

castWireValue :: (MonadFail m, Wireable a) => WireValue -> m a
castWireValue (WireValue x) = cast' x

(<|$|>) :: (Wireable a, MonadFail m) => (a -> b) -> WireValue -> m b
f <|$|> wv = f <$> castWireValue wv

(<|*|>) :: (Wireable a, MonadFail m) => m (a -> b) -> WireValue -> m b
mf <|*|> wv = mf <*> castWireValue wv

-- Wireable serialisation:

-- | We define a type for tags that we want to use to denote our types on the
--   wire, so that we can define functions that we can verify are total.
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

withWireConcreteTypeProxy
  :: (forall a. Wireable a => Proxy a -> r) -> WireConcreteType -> r
withWireConcreteTypeProxy f t = case t of
  WcTime -> f (Proxy :: Proxy Time)
  WcWord8 -> f (Proxy :: Proxy Word8)
  WcWord32 -> f (Proxy :: Proxy Word32)
  WcWord64 -> f (Proxy :: Proxy Word64)
  WcInt32 -> f (Proxy :: Proxy Int32)
  WcInt64 -> f (Proxy :: Proxy Int64)
  WcFloat -> f (Proxy :: Proxy Float)
  WcDouble -> f (Proxy :: Proxy Double)
  WcString -> f (Proxy :: Proxy Text)

withWireTypeProxy
  :: forall r. (forall a. Wireable a => Proxy a -> r) -> WireType -> r
withWireTypeProxy f wt = case wt of
    WtConc concT -> withWireConcreteTypeProxy f concT
    _ -> let (concT, contTs) = unpackWt wt in
      withWireConcreteTypeProxy (applyUnpacked contTs) concT
  where
    unpackWt :: WireType -> (WireConcreteType, [WireContainerType])
    unpackWt wt = let (c, ts) = inner wt in (c, reverse ts)
      where
        inner (WtConc t) = (t, [])
        inner (WtCont w s) = let (c, ts) = inner s in (c, w : ts)
    applyUnpacked :: forall a. Wireable a => [WireContainerType] -> Proxy a -> r
    applyUnpacked [] p = f p
    applyUnpacked (ct:cts) _ = case ct of
      WcList -> applyUnpacked cts (Proxy :: Proxy [a])

-- FIXME: could use an association list type that checks the uniqueness of the
-- keys on creation:
revAssoc :: (Enum a, Bounded a) => (a -> r) -> [(r, a)]
revAssoc f = [(f e, e) | e <- [minBound..]]


concWord :: WireConcreteType -> Word8
concWord t = case t of
  WcTime -> [btq|t|]
  WcWord8 -> [btq|b|]
  WcWord32 -> [btq|w|]
  WcWord64 -> [btq|W|]
  WcInt32 -> [btq|i|]
  WcInt64 -> [btq|I|]
  WcFloat -> [btq|f|]
  WcDouble -> [btq|F|]
  WcString -> [btq|s|]

wordConcs :: [(Word8, WireConcreteType)]
wordConcs = revAssoc concWord

wordConc :: MonadFail m => Word8 -> m WireConcreteType
wordConc w =
  maybe (fail "Unrecognised concrete type tag") return $ lookup w wordConcs

contWord :: WireContainerType -> Word8
contWord t = case t of
  WcList -> [btq|l|]

wordConts :: [(Word8, WireContainerType)]
wordConts = revAssoc contWord

wordCont :: MonadFail m => Word8 -> m WireContainerType
wordCont w =
  maybe (fail "Unrecognised container type tag") return $ lookup w wordConts

concTyCon :: WireConcreteType -> TyCon
concTyCon = withWireConcreteTypeProxy $ typeRepTyCon . typeRep

tyConConcs :: [(TyCon, WireConcreteType)]
tyConConcs = revAssoc concTyCon

wireValueWireType :: WireValue -> WireType
wireValueWireType (WireValue a) = inner $ typeOf a
  where
    inner wt
      | tc == listTyCon = WtCont WcList $ inner $ head $ typeRepArgs wt
      | otherwise = WtConc $ fromJust $ lookup tc tyConConcs
      where tc = typeRepTyCon wt
    listTyCon = typeRepTyCon $ typeRep (Proxy :: Proxy [()])

instance Encodable WireConcreteType where
  encode = return . fromWord8 . concWord
  decode = anyWord8 >>= wordConc

instance Encodable WireType where
  encode (WtConc t) = encode t
  encode (WtCont w s) = ((fromWord8 $ contWord w) <>) <$> encode s
  decode = anyWord8 >>= inner
    where
      inner w = (WtConc <$> wordConc w) <|> (WtCont <$> wordCont w <*> decode)

instance Encodable WireValue where
  encode wv@(WireValue a) = (<>) <$> encode (wireValueWireType wv) <*> encode a
  decode = decode >>= withWireTypeProxy go
    where
      go :: forall a. Wireable a => Proxy a -> Parser WireValue
      go _ = WireValue <$> decode @a

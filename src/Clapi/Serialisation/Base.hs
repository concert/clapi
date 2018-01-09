{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Clapi.Serialisation.Base where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad (liftM2)

import Data.Word
import Data.Int
import Data.Text (Text)

-- For building:
import Blaze.ByteString.Builder
  ( Builder, fromWord8, fromWord16be, fromWord64be, fromWord32be , fromInt32be
  , fromInt64be)
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Data.ByteString.Builder (floatBE, doubleBE)
import Data.Monoid

-- For parsing:
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.ByteString (Parser, anyWord8, count)
import Data.Attoparsec.Binary (anyWord16be, anyWord32be, anyWord64be)
import Data.Binary.IEEE754 (wordToFloat, wordToDouble)
import Data.Text.Encoding (decodeUtf8With)

import Clapi.Types.Base (Time(..), TimeStamped(..), Tag(..), mkTag)
import Clapi.Types.ByteTagQ (btq)
import Clapi.Util (bound)

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

-- ALINEALINEALINEALINEALINE

(<<>>) :: (Monad m) => m Builder -> m Builder -> m Builder
(<<>>) = liftM2 (<>)

instance Encodable Tag where
    encode = return . fromWord8 . unTag
    decode = anyWord8 >>= mkTag

instance (Encodable a) => Encodable (Maybe a) where
    encode (Just a) = encode [btq|J|] <<>> encode a
    encode Nothing = encode [btq|N|]
    decode = decode >>= unpack
      where
        unpack :: Tag -> Parser (Maybe a)
        unpack t | t == [btq|J|] = Just <$> decode
                 | t == [btq|N|] = return Nothing
                 | otherwise = fail $ "Bad maybe type tag " ++ show t

instance (Encodable a) => Encodable (TimeStamped a) where
    encode (TimeStamped (t, a)) = encode t <<>> encode a
    decode = curry TimeStamped <$> decode <*> decode

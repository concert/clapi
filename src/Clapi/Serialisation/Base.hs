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
import Clapi.Types.UniqList (UniqList, mkUniqList, unUniqList)
import Clapi.TH (btq)
import Clapi.Util (bound)

class Encodable a where
  builder :: MonadFail m => a -> m Builder
  parser :: Parser a

instance Encodable Time where
  builder (Time w64 w32) = return $ fromWord64be w64 <> fromWord32be w32
  parser = Time <$> anyWord64be <*> anyWord32be

instance Encodable Word8 where
  builder = return . fromWord8
  parser = anyWord8
instance Encodable Word32 where
  builder = return . fromWord32be
  parser = anyWord32be
instance Encodable Word64 where
  builder = return . fromWord64be
  parser = anyWord64be

instance Encodable Int32 where
  builder = return . fromInt32be
  parser = fromIntegral <$> anyWord32be
instance Encodable Int64 where
  builder = return . fromInt64be
  parser = fromIntegral <$> anyWord64be

instance Encodable Float where
  builder = return . floatBE
  parser = wordToFloat <$> anyWord32be
instance Encodable Double where
  builder = return . doubleBE
  parser = wordToDouble <$> anyWord64be

instance Encodable Text where
  builder t = return $ fromText t <> fromWord8 0
  parser = DAB.takeWhile (/= 0) <* anyWord8 >>= return . decodeUtf8With onError
    where
      onError :: String -> Maybe Word8 -> Maybe Char
      onError _ Nothing = Nothing  -- Unexpected end of Input
      onError _ _ = Just '?'  -- Undecodable

instance Encodable a => Encodable [a] where
  builder = listBuilder builder
  parser = listParser parser

listBuilder :: MonadFail m => (a -> m Builder) -> [a] -> m Builder
listBuilder enc as = do
    len <- bound (length as)
    builders <- mapM enc as
    return $ fromWord16be len <> mconcat builders

listParser :: Parser a -> Parser [a]
listParser dec = do
    len <- anyWord16be
    count (fromIntegral len) dec

instance (Encodable a, Ord a, Show a) => Encodable (UniqList a) where
  builder = builder . unUniqList
  parser = parser >>= mkUniqList

-- ALINEALINEALINEALINEALINE

(<<>>) :: (Monad m) => m Builder -> m Builder -> m Builder
(<<>>) = liftM2 (<>)

instance Encodable Tag where
    builder = return . fromWord8 . unTag
    parser = anyWord8 >>= mkTag

instance (Encodable a) => Encodable (Maybe a) where
    builder (Just a) = builder [btq|J|] <<>> builder a
    builder Nothing = builder [btq|N|]
    parser = parser >>= unpack
      where
        unpack :: Tag -> Parser (Maybe a)
        unpack t | t == [btq|J|] = Just <$> parser
                 | t == [btq|N|] = return Nothing
                 | otherwise = fail $ "Bad maybe type tag " ++ show t

instance (Encodable a) => Encodable (TimeStamped a) where
    builder (TimeStamped (t, a)) = builder t <<>> builder a
    parser = curry TimeStamped <$> parser <*> parser

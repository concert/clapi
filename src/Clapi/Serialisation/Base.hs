{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , LambdaCase
  , StandaloneDeriving
#-}

module Clapi.Serialisation.Base where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad (liftM2)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tagged (Tagged(..))

import Data.Word
import Data.Int
import Data.Text (Text)

-- For building:
import Blaze.ByteString.Builder
  ( Builder, fromWord8, fromWord64be, fromWord32be , fromInt32be
  , fromInt64be)
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Data.ByteString.Builder (floatBE, doubleBE)
import qualified Data.ByteString.Builder.VarWord as BVw
import Data.Monoid

-- For parsing:
import qualified Data.Attoparsec.ByteString as DAB
import qualified Data.Attoparsec.VarWord as AVw
import Data.Attoparsec.ByteString (Parser, anyWord8, count)
import Data.Attoparsec.Binary (anyWord32be, anyWord64be)
import Data.Binary.IEEE754 (wordToFloat, wordToDouble)
import Data.Text.Encoding (decodeUtf8With)

import Clapi.TaggedData
  (TaggedData, taggedData, tdInstanceToTag, tdAllTags, tdTagToEnum)
import Clapi.Types.AssocList (AssocList, mkAssocList, unAssocList)
import Clapi.Types.Base
  ( Time(..), TimeStamped(..), Tag(..), mkTag, InterpolationLimit(..)
  , Interpolation(..), InterpolationType(..), interpolationType)
import Clapi.Types.UniqList (UniqList, mkUniqList, unUniqList)
import Clapi.TH (btq)

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

deriving instance Encodable a => Encodable (Tagged t a)

instance Encodable a => Encodable [a] where
  builder = listBuilder builder
  parser = listParser parser

listBuilder :: MonadFail m => (a -> m Builder) -> [a] -> m Builder
listBuilder enc as = do
    builders <- mapM enc as
    return $ BVw.denseVarWordBe (length as) <> mconcat builders

listParser :: Parser a -> Parser [a]
listParser dec = do
    len <- AVw.denseVarWordBe
    count len dec

instance (Encodable a, Ord a, Show a) => Encodable (UniqList a) where
  builder = builder . unUniqList
  parser = parser >>= mkUniqList

instance (Ord k, Encodable k, Encodable v) => Encodable (Map k v) where
  builder = builder . Map.toList
  parser = Map.fromList <$> parser

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

instance (Encodable a, Encodable b) => Encodable (a, b) where
    builder (a, b) = builder a <<>> builder b
    parser = (,) <$> parser <*> parser

instance (Ord k, Show k, Encodable k, Encodable v)
  => Encodable (AssocList k v) where
    builder = builder . unAssocList
    parser = parser >>= mkAssocList


tdTaggedParser :: TaggedData e a -> (e -> Parser a) -> Parser a
tdTaggedParser td p = let at = tdAllTags td in do
    t <- parser
    if t `elem` at
      then p $ tdTagToEnum td t
      else fail $ "Invalid tag " ++ show t ++ " valid tags are " ++ show at

tdTaggedBuilder
  :: MonadFail m =>TaggedData e a -> (a -> m Builder) -> a -> m Builder
tdTaggedBuilder td bdr a = builder (tdInstanceToTag td $ a) <<>> bdr a


ilToTag :: InterpolationLimit -> Tag
ilToTag il = case il of
    ILUninterpolated -> [btq|U|]
    ILConstant -> [btq|C|]
    ILLinear -> [btq|L|]
    ILBezier -> [btq|B|]

ilTaggedData :: TaggedData InterpolationLimit InterpolationLimit
ilTaggedData = taggedData ilToTag id

instance Encodable InterpolationLimit where
  builder = tdTaggedBuilder ilTaggedData $ const $ return mempty
  parser = tdTaggedParser ilTaggedData return

itToTag :: InterpolationType -> Tag
itToTag it = case it of
    ItConstant -> [btq|c|]
    ItLinear -> [btq|l|]
    ItBezier -> [btq|b|]

interpolationTaggedData :: TaggedData InterpolationType Interpolation
interpolationTaggedData = taggedData itToTag interpolationType

instance Encodable Interpolation where
    builder = tdTaggedBuilder interpolationTaggedData $ \i -> return $ case i of
        (IBezier a b) -> fromWord32be a <> fromWord32be b
        _ -> mempty
    parser = tdTaggedParser interpolationTaggedData $ \e -> case e of
        ItConstant -> return IConstant
        ItLinear -> return ILinear
        ItBezier -> IBezier <$> parser <*> parser

data LR = L | R deriving (Enum, Bounded)

eitherTaggedData :: TaggedData LR (Either a b)
eitherTaggedData = taggedData typeToTag valueToType
  where
    typeToTag L = [btq|<|]
    typeToTag R = [btq|>|]
    valueToType (Left _) = L
    valueToType (Right _) = R

instance (Encodable a, Encodable b) => Encodable (Either a b) where
  builder = tdTaggedBuilder eitherTaggedData $ \case
    Left a -> builder a
    Right b -> builder b
  parser = tdTaggedParser eitherTaggedData $ \case
    L -> Left <$> parser
    R -> Right <$> parser

instance Encodable () where
  builder _ = return mempty
  parser = return ()

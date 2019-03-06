{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , StandaloneDeriving
#-}

module Clapi.Serialisation.Base where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad (liftM2)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tagged (Tagged(..))

import Data.Word
import Data.Int
import Data.Text (Text)

import Data.Map.Mol (Mol(..))
import Data.Map.Mos (Mos)
import qualified Data.Map.Mos as Mos

-- For building:
import Blaze.ByteString.Builder
  ( Builder, fromWord8, fromInt32be, fromInt64be)
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Data.ByteString.Builder (floatBE, doubleBE)
import qualified Data.ByteString.Builder.VarWord as BVw

-- For parsing:
import qualified Data.Attoparsec.ByteString as DAB
import qualified Data.Attoparsec.VarWord as AVw
import Data.Attoparsec.ByteString (Parser, anyWord8, count)
import Data.Attoparsec.Binary (anyWord32be, anyWord64be)
import Data.Binary.IEEE754 (wordToFloat, wordToDouble)
import Data.Text.Encoding (decodeUtf8With)

import Clapi.TaggedData
  (TaggedData, taggedData, tdInstanceToTag, tdAllTags, tdTagToEnum)
import Clapi.Types.AssocList (AssocList)
import qualified Clapi.Types.AssocList as AL
import Clapi.Types.Base
  ( Attributee(..), Time(..), TimeStamped(..), Tag(..), mkTag
  , Interpolation(..), InterpolationType(..)
  , typeEnumOf
  )
import Clapi.Types.UniqList (UniqList, mkUniqList, unUniqList)
import Clapi.TH (btq)

class Encodable a where
  builder :: MonadFail m => a -> m Builder

class Decodable a where
  parser :: Parser a

instance Encodable Time where
  builder (Time w64 w32) = return $
    BVw.denseVarWordBe w64 <> BVw.denseVarWordBe w32
instance Decodable Time where
  parser = Time <$> AVw.denseVarWordBe <*> AVw.denseVarWordBe

instance Encodable Word32 where
  builder = return . BVw.denseVarWordBe
instance Decodable Word32 where
  parser = AVw.denseVarWordBe

instance Encodable Word64 where
  builder = return . BVw.denseVarWordBe
instance Decodable Word64 where
  parser = AVw.denseVarWordBe

instance Encodable Int32 where
  builder = return . fromInt32be
instance Decodable Int32 where
  parser = fromIntegral <$> anyWord32be

instance Encodable Int64 where
  builder = return . fromInt64be
instance Decodable Int64 where
  parser = fromIntegral <$> anyWord64be

instance Encodable Float where
  builder = return . floatBE
instance Decodable Float where
  parser = wordToFloat <$> anyWord32be

instance Encodable Double where
  builder = return . doubleBE
instance Decodable Double where
  parser = wordToDouble <$> anyWord64be

instance Encodable Text where
  builder t = return $ fromText t <> fromWord8 0
instance Decodable Text where
  parser = DAB.takeWhile (/= 0) <* anyWord8 >>= return . decodeUtf8With onError
    where
      onError :: String -> Maybe Word8 -> Maybe Char
      onError _ Nothing = Nothing  -- Unexpected end of Input
      onError _ _ = Just '?'  -- Undecodable

deriving instance Encodable a => Encodable (Tagged t a)
deriving instance Decodable a => Decodable (Tagged t a)

instance Encodable a => Encodable [a] where
  builder = listBuilder builder
instance Decodable a => Decodable [a] where
  parser = listParser parser

listBuilder :: MonadFail m => (a -> m Builder) -> [a] -> m Builder
listBuilder enc as = do
    builders <- mapM enc as
    return $ BVw.denseVarWordBe (length as) <> mconcat builders

listParser :: Parser a -> Parser [a]
listParser dec = do
    len <- AVw.denseVarWordBe
    count len dec

instance Encodable a => Encodable (UniqList a) where
  builder = builder . unUniqList
instance (Ord a, Show a, Decodable a) => Decodable (UniqList a) where
  parser = parser >>= mkUniqList

instance (Encodable k, Encodable v) => Encodable (Map k v) where
  builder = builder . Map.toList
instance (Ord k, Decodable k, Show k, Decodable v) => Decodable (Map k v) where
  parser = AL.toMap <$> parser

deriving instance (Encodable k, Encodable v) => Encodable (Mol k v)
deriving instance (Ord k, Decodable k, Show k, Decodable v) => Decodable (Mol k v)


instance Encodable a => Encodable (Set a) where
  builder = builder . Set.toList
instance (Ord a, Decodable a) => Decodable (Set a) where
  parser = Set.fromList <$> parser

instance (Encodable k, Encodable v) => Encodable (Mos k v) where
  builder = builder . Mos.toList
instance (Ord k, Ord v, Decodable k, Decodable v) => Decodable (Mos k v) where
  parser = Mos.fromList <$> parser

(<<>>) :: (Monad m) => m Builder -> m Builder -> m Builder
(<<>>) = liftM2 (<>)

instance Encodable Tag where
    builder = return . fromWord8 . unTag
instance Decodable Tag where
    parser = anyWord8 >>= mkTag

instance (Encodable a) => Encodable (Maybe a) where
    builder (Just a) = builder [btq|J|] <<>> builder a
    builder Nothing = builder [btq|N|]
instance Decodable a => Decodable (Maybe a) where
    parser = parser >>= unpack
      where
        unpack :: Tag -> Parser (Maybe a)
        unpack t | t == [btq|J|] = Just <$> parser
                 | t == [btq|N|] = return Nothing
                 | otherwise = fail $ "Bad maybe type tag " ++ show t

instance (Encodable a) => Encodable (TimeStamped a) where
    builder (TimeStamped (t, a)) = builder t <<>> builder a
instance Decodable a => Decodable (TimeStamped a) where
    parser = curry TimeStamped <$> parser <*> parser

instance (Encodable a, Encodable b) => Encodable (a, b) where
    builder (a, b) = builder a <<>> builder b
instance (Decodable a, Decodable b) => Decodable (a, b) where
    parser = (,) <$> parser <*> parser

instance (Encodable k, Encodable v) => Encodable (AssocList k v) where
    builder = builder . AL.unAssocList
instance (Ord k, Show k, Decodable k, Decodable v)
  => Decodable (AssocList k v) where
    parser = parser >>= AL.mkAssocList


deriving instance Encodable Attributee
deriving instance Decodable Attributee


tdTaggedParser :: TaggedData e a -> (e -> Parser a) -> Parser a
tdTaggedParser td p = let at = tdAllTags td in do
    t <- parser
    if t `elem` at
      then p $ tdTagToEnum td t
      else fail $ "Invalid tag " ++ show t ++ " valid tags are " ++ show at

tdTaggedBuilder
  :: MonadFail m => TaggedData e a -> (a -> m Builder) -> a -> m Builder
tdTaggedBuilder td bdr a = builder (tdInstanceToTag td $ a) <<>> bdr a

itToTag :: InterpolationType -> Tag
itToTag it = case it of
    ItConstant -> [btq|c|]
    ItLinear -> [btq|l|]
    ItBezier -> [btq|b|]

itTaggedData :: TaggedData InterpolationType InterpolationType
itTaggedData = taggedData itToTag id

instance Encodable InterpolationType where
  builder = tdTaggedBuilder itTaggedData $ const $ return mempty
instance Decodable InterpolationType where
  parser = tdTaggedParser itTaggedData return

interpolationTaggedData :: TaggedData InterpolationType Interpolation
interpolationTaggedData = taggedData itToTag typeEnumOf

instance Encodable Interpolation where
    builder = tdTaggedBuilder interpolationTaggedData $ \i -> return $ case i of
        (IBezier a b) -> BVw.denseVarWordBe a <> BVw.denseVarWordBe b
        _ -> mempty
instance Decodable Interpolation where
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
instance (Decodable a, Decodable b) => Decodable (Either a b) where
  parser = tdTaggedParser eitherTaggedData $ \case
    L -> Left <$> parser
    R -> Right <$> parser

instance Encodable () where
  builder _ = return mempty
instance Decodable () where
  parser = return ()

{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module Clapi.Serialisation.Wire where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Control.Applicative ((<|>))
import Data.Word
import Data.Int
import Data.Text (Text)
import Data.Typeable
import Data.Maybe (fromJust)

import Data.Monoid

import Data.Attoparsec.ByteString (Parser)

import Clapi.Serialisation.Base (Encodable(..), (<<>>))
import Clapi.Types.Base (Tag, Time)
import Clapi.Types.Wire (WireValue(..), Wireable)
import Clapi.TH (btq)
import Clapi.Util (proxyF, proxyF3)

-- | We define a type for tags that we want to use to denote our types on the
--   wire, so that we can define functions that we can verify are total.
data WireTypeName
  = WtnTime
  | WtnWord8 | WtnWord32 | WtnWord64
  | WtnInt32 | WtnInt64
  | WtnFloat | WtnDouble
  | WtnString
  | WtnList
  | WtnMaybe
  | WtnPair
  deriving (Show, Eq, Ord, Enum, Bounded)

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

wtName :: WireType -> WireTypeName
wtName wt = case wt of
  WtTime -> WtnTime
  WtWord8 -> WtnWord8
  WtWord32 -> WtnWord32
  WtWord64 -> WtnWord64
  WtInt32 -> WtnInt32
  WtInt64 -> WtnInt64
  WtFloat -> WtnFloat
  WtDouble -> WtnDouble
  WtString -> WtnString
  WtList _ -> WtnList
  WtMaybe _ -> WtnMaybe
  WtPair _ _ -> WtnPair

withWtProxy :: WireType -> (forall a. Wireable a => Proxy a -> r) -> r
withWtProxy wt f = case wt of
  WtTime -> f $ Proxy @Time
  WtWord8 -> f $ Proxy @Word8
  WtWord32 -> f $ Proxy @Word32
  WtWord64 -> f $ Proxy @Word64
  WtInt32 -> f $ Proxy @Int32
  WtInt64 -> f $ Proxy @Int64
  WtFloat -> f $ Proxy @Float
  WtDouble -> f $ Proxy @Double
  WtString -> f $ Proxy @Text
  WtList wt' -> withWtProxy wt' $ f . proxyF (Proxy @[])
  WtMaybe wt' -> withWtProxy wt' $ f . proxyF (Proxy @Maybe)
  WtPair wt1 wt2 ->
    withWtProxy wt1 $ \p1 ->
      withWtProxy wt2 $ \p2 ->
        f $ proxyF3 (Proxy @(,)) p1 p2

wtnTag :: WireTypeName -> Tag
wtnTag wt = case wt of
  WtnTime -> [btq|t|]
  WtnWord8 -> [btq|b|]
  WtnWord32 -> [btq|w|]
  WtnWord64 -> [btq|W|]
  WtnInt32 -> [btq|i|]
  WtnInt64 -> [btq|I|]
  WtnFloat -> [btq|f|]
  WtnDouble -> [btq|F|]
  WtnString -> [btq|s|]
  WtnList -> [btq|l|]
  WtnMaybe -> [btq|m|]
  WtnPair -> [btq|p|]

tagWtns :: [(Tag, WireTypeName)]
tagWtns = revAssoc wtnTag

tagWtn :: MonadFail m => Tag -> m WireTypeName
tagWtn t = maybe (fail "Unrecognised type tag") return $ lookup t tagWtns

-- FIXME: could use an association list type that checks the uniqueness of the
-- keys on creation:
revAssoc :: (Enum a, Bounded a) => (a -> r) -> [(r, a)]
revAssoc f = [(f e, e) | e <- [minBound..]]

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

instance Encodable WireTypeName where
  builder = builder . wtnTag
  parser = parser >>= tagWtn

instance Encodable WireType where
  builder wt =
    builder (wtnTag $ wtName wt) <<>> case wt of
      WtList wt' -> builder wt'
      WtMaybe wt' -> builder wt'
      WtPair wt1 wt2 -> builder wt1 <<>> builder wt2
      _ -> return mempty
  parser = parser >>= go
    where
      go :: WireTypeName -> Parser WireType
      go wtn = case wtn of
        WtnTime -> return WtTime
        WtnWord8 -> return WtWord8
        WtnWord32 -> return WtWord32
        WtnWord64 -> return WtWord64
        WtnInt32 -> return WtInt32
        WtnInt64 -> return WtInt64
        WtnFloat -> return WtFloat
        WtnDouble -> return WtDouble
        WtnString -> return WtString
        WtnList -> WtList <$> parser
        WtnMaybe -> WtMaybe <$> parser
        WtnPair -> WtPair <$> parser <*> parser

instance Encodable WireValue where
  builder wv@(WireValue a) =
    (<>) <$> builder (wireValueWireType wv) <*> builder a
  parser = parser >>= \wt -> withWtProxy wt go
    where
      go :: forall a. Wireable a => Proxy a -> Parser WireValue
      go _ = WireValue <$> parser @a

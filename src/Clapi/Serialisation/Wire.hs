{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE
    ScopedTypeVariables
  , QuasiQuotes
  , TypeApplications
#-}

module Clapi.Serialisation.Wire where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Data.Typeable

import Data.Monoid

import Data.Attoparsec.ByteString (Parser)

import Clapi.Serialisation.Base (Encodable(..), (<<>>))
import Clapi.Types.Base (Tag)
import Clapi.Types.Wire
  (WireValue(..), Wireable, WireType(..), wireValueWireType, withWtProxy)
import Clapi.TH (btq)

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

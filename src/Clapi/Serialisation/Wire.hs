{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    FlexibleInstances
  , GADTs
  , LambdaCase
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , TypeOperators
#-}

module Clapi.Serialisation.Wire where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Data.Constraint (Dict(..), mapDict, (:=>)(..), (:-)(..))
import Data.Typeable

import Data.Attoparsec.ByteString (Parser)

import Clapi.Serialisation.Base (Encodable(..), (<<>>))
import Clapi.Types.Base (Tag)
import Clapi.Types.Wire (WireValue(..), WireType(..), SomeWireType(..))
import Clapi.TH (btq)

-- | We define a type for tags that we want to use to denote our types on the
--   wire, so that we can define functions that we can verify are total.
data WireTypeName
  = WtnTime
  | WtnWord32 | WtnWord64
  | WtnInt32 | WtnInt64
  | WtnFloat | WtnDouble
  | WtnString
  | WtnList
  | WtnMaybe
  | WtnPair
  deriving (Show, Eq, Ord, Enum, Bounded)

wtName :: WireType a -> WireTypeName
wtName = \case
  WtTime -> WtnTime
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
wtnTag = \case
  WtnTime -> [btq|t|]
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

instance Encodable SomeWireType where
  builder (SomeWireType wt) =
    builder (wtnTag $ wtName wt) <<>> case wt of
      WtList wt' -> builder (SomeWireType wt')
      WtMaybe wt' -> builder (SomeWireType wt')
      WtPair wt1 wt2 ->
        builder (SomeWireType wt1) <<>> builder (SomeWireType wt2)
      _ -> return mempty
  parser = parser >>= go
    where
      go :: WireTypeName -> Parser SomeWireType
      go wtn = case wtn of
        WtnTime -> rtn WtTime
        WtnWord32 -> rtn WtWord32
        WtnWord64 -> rtn WtWord64
        WtnInt32 -> rtn WtInt32
        WtnInt64 -> rtn WtInt64
        WtnFloat -> rtn WtFloat
        WtnDouble -> rtn WtDouble
        WtnString -> rtn WtString
        WtnList -> do
          SomeWireType wt <- parser
          rtn $ WtList wt
        WtnMaybe -> do
          SomeWireType wt <- parser
          rtn $ WtMaybe wt
        WtnPair -> do
          SomeWireType wt1 <- parser
          SomeWireType wt2 <- parser
          rtn $ WtPair wt1 wt2
      rtn :: WireType a -> Parser SomeWireType
      rtn = return . SomeWireType

getEncodable :: WireType a -> Dict (Encodable a)
getEncodable = \case
  WtTime -> Dict
  WtWord32 -> Dict
  WtList wt -> mapDict ins $ getEncodable wt
  WtPair wt1 wt2 -> pairDict (getEncodable wt1) (getEncodable wt2)

pairDict :: Dict (Encodable a) -> Dict (Encodable b) -> Dict (Encodable (a, b))
pairDict Dict Dict = Dict

instance Encodable x :=> Encodable [x] where
  ins = Sub Dict

instance Encodable WireValue where
  builder (WireValue wt a) =
    (<>) <$> builder (SomeWireType wt) <*> case getEncodable wt of
      Dict -> builder a
  parser = parser >>= \(SomeWireType wt) -> case getEncodable wt of
    Dict -> parser

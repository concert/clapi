{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Clapi.Serialisation.Wire where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Control.Applicative ((<|>))
import Data.Word
import Data.Int
import Data.Text (Text)
import Data.Typeable
import Data.Maybe (fromJust)

-- For building:
import Data.Monoid

-- For parsing:
import Data.Attoparsec.ByteString (Parser)

import Clapi.Serialisation.Base (Encodable(..), (<<>>))
import Clapi.Types.Base (Tag, Time)
import Clapi.Types.Wire (WireValue(..), Wireable)
import Clapi.Types.ByteTagQ (btq)

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

unpackWireType :: WireType -> (WireConcreteType, [WireContainerType])
unpackWireType wt = let (c, ts) = inner wt in (c, reverse ts)
  where
    inner (WtConc t) = (t, [])
    inner (WtCont w s) = let (c, ts) = inner s in (c, w : ts)

withWireConcreteTypeProxy
  :: (forall a. Wireable a => Proxy a -> r) -> WireConcreteType -> r
withWireConcreteTypeProxy f t = case t of
  WcTime -> f (Proxy @Time)
  WcWord8 -> f (Proxy @Word8)
  WcWord32 -> f (Proxy @Word32)
  WcWord64 -> f (Proxy @Word64)
  WcInt32 -> f (Proxy @Int32)
  WcInt64 -> f (Proxy @Int64)
  WcFloat -> f (Proxy @Float)
  WcDouble -> f (Proxy @Double)
  WcString -> f (Proxy @Text)

withWireTypeProxy
  :: forall r. (forall a. Wireable a => Proxy a -> r) -> WireType -> r
withWireTypeProxy f wt = case wt of
    WtConc concT -> withWireConcreteTypeProxy f concT
    _ -> let (concT, contTs) = unpackWireType wt in
      withWireConcreteTypeProxy (applyUnpacked contTs) concT
  where
    applyUnpacked :: forall a. Wireable a => [WireContainerType] -> Proxy a -> r
    applyUnpacked [] p = f p
    applyUnpacked (ct:cts) _ = case ct of
      WcList -> applyUnpacked cts (Proxy :: Proxy [a])

-- FIXME: could use an association list type that checks the uniqueness of the
-- keys on creation:
revAssoc :: (Enum a, Bounded a) => (a -> r) -> [(r, a)]
revAssoc f = [(f e, e) | e <- [minBound..]]


concTag :: WireConcreteType -> Tag
concTag t = case t of
  WcTime -> [btq|t|]
  WcWord8 -> [btq|b|]
  WcWord32 -> [btq|w|]
  WcWord64 -> [btq|W|]
  WcInt32 -> [btq|i|]
  WcInt64 -> [btq|I|]
  WcFloat -> [btq|f|]
  WcDouble -> [btq|F|]
  WcString -> [btq|s|]

tagConcs :: [(Tag, WireConcreteType)]
tagConcs = revAssoc concTag

tagConc :: MonadFail m => Tag -> m WireConcreteType
tagConc w =
  maybe (fail "Unrecognised concrete type tag") return $ lookup w tagConcs

contTag :: WireContainerType -> Tag
contTag t = case t of
  WcList -> [btq|l|]

tagConts :: [(Tag, WireContainerType)]
tagConts = revAssoc contTag

tagCont :: MonadFail m => Tag -> m WireContainerType
tagCont w =
  maybe (fail "Unrecognised container type tag") return $ lookup w tagConts

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
  builder = builder . concTag
  parser = parser >>= tagConc

instance Encodable WireType where
  builder (WtConc t) = builder t
  builder (WtCont w s) = (builder $ contTag w) <<>> builder s
  parser = parser >>= inner
    where
      inner w = (WtConc <$> tagConc w) <|> (WtCont <$> tagCont w <*> parser)

instance Encodable WireValue where
  builder wv@(WireValue a) = (<>) <$> builder (wireValueWireType wv) <*> builder a
  parser = parser >>= withWireTypeProxy go
    where
      go :: forall a. Wireable a => Proxy a -> Parser WireValue
      go _ = WireValue <$> parser @a

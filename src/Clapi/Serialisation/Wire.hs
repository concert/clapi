{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    GADTs
  , LambdaCase
  , TemplateHaskell
#-}

module Clapi.Serialisation.Wire where

import Data.Constraint (Dict(..))
import Language.Haskell.TH (Type(ConT))

import Clapi.Serialisation.Base
  (Encodable(..), Decodable(..), (<<>>), tdTaggedBuilder, tdTaggedParser)
import Clapi.TaggedData (TaggedData, taggedData)
import Clapi.Types.Base (Tag, TypeEnumOf(..))
import Clapi.Types.Wire
  ( WireType(..), SomeWireType(..), withWireType, WireTypeName(..)
  , wtTime, wtWord32, wtWord64, wtInt32, wtInt64, wtFloat, wtDouble, wtString
  , wtList, wtMaybe, wtPair
  , WireValue(..), SomeWireValue(..), withWireValue, someWv)
import Clapi.Types.WireTH (mkGetWtConstraint)
import Clapi.TH (btq)

wtnTag :: WireTypeName -> Tag
wtnTag wt = case wt of
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

wtnTaggedData :: TaggedData WireTypeName (WireType a)
wtnTaggedData = taggedData wtnTag typeEnumOf

instance Encodable (WireType a) where
  builder = tdTaggedBuilder wtnTaggedData $ \case
    WtList wt -> builder wt
    WtMaybe wt -> builder wt
    WtPair wt1 wt2 -> builder wt1 <<>> builder wt2
    _ -> return mempty

someWtnTaggedData :: TaggedData WireTypeName SomeWireType
someWtnTaggedData = taggedData wtnTag typeEnumOf

instance Encodable SomeWireType where
  builder = withWireType builder
instance Decodable SomeWireType where
  parser = tdTaggedParser someWtnTaggedData $ \case
    WtnTime -> return wtTime
    WtnWord32 -> return wtWord32
    WtnWord64 -> return wtWord64
    WtnInt32 -> return wtInt32
    WtnInt64 -> return wtInt64
    WtnFloat -> return wtFloat
    WtnDouble -> return wtDouble
    WtnString -> return wtString
    WtnList -> wtList <$> parser
    WtnMaybe -> wtMaybe <$> parser
    WtnPair -> wtPair <$> parser <*> parser

mkGetWtConstraint "getEncodable" $ ConT ''Encodable
mkGetWtConstraint "getDecodable" $ ConT ''Decodable

instance Encodable (WireValue a) where
  builder (WireValue wt a) = builder wt <<>>
    case getEncodable wt of Dict -> builder a

instance Encodable SomeWireValue where
  builder = withWireValue builder
instance Decodable SomeWireValue where
  parser = do
    SomeWireType wt <- parser
    case getDecodable wt of
      Dict -> someWv wt <$> parser

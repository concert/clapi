{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    GADTs
#-}

module Clapi.Serialisation.Definitions where

import Clapi.Serialisation.Base
  ( Encodable(..), Decodable(..), (<<>>), tdTaggedBuilder, tdTaggedParser)
import Clapi.Serialisation.Name ()
import Clapi.Serialisation.Path ()
import Clapi.TaggedData (TaggedData, taggedData)
import Clapi.TextSerialisation (ttToText, ttFromText)
import Clapi.TH (btq)
import Clapi.Types.Base (TypeEnumOf(..))
import Clapi.Types.Definitions
  ( Editability(..), MetaType(..)
  , Definition(..), SomeDefinition(..), tupleDef, structDef, arrayDef
  , PostDefinition(..))
import Clapi.Types.Tree (TreeType, SomeTreeType, withTreeType)

editableTaggedData :: TaggedData Editability Editability
editableTaggedData = taggedData toTag id
  where
    toTag r = case r of
      Editable -> [btq|w|]
      ReadOnly -> [btq|r|]

instance Encodable Editability where
  builder = tdTaggedBuilder editableTaggedData $ const $ return mempty
instance Decodable Editability where
  parser = tdTaggedParser editableTaggedData return

-- FIXME: do we want to serialise the type to text first?!
instance Encodable (TreeType a) where
  builder = builder . ttToText

instance Encodable SomeTreeType where
  builder = withTreeType builder
instance Decodable SomeTreeType where
  parser = parser >>= ttFromText

defTaggedData :: TaggedData MetaType SomeDefinition
defTaggedData = taggedData typeToTag typeEnumOf
  where
    typeToTag mt = case mt of
      Tuple -> [btq|T|]
      Struct -> [btq|S|]
      Array -> [btq|A|]

instance Encodable SomeDefinition where
  builder = tdTaggedBuilder defTaggedData $ \(SomeDefinition d) -> case d of
    TupleDef doc tys ilimit -> builder doc <<>> builder tys <<>> builder ilimit
    StructDef doc tyinfo -> builder doc <<>> builder tyinfo
    ArrayDef doc ptn tn ed ->
      builder doc <<>> builder ptn <<>> builder tn <<>> builder ed
instance Decodable SomeDefinition where
  parser = tdTaggedParser defTaggedData $ \mt -> case mt of
    Tuple -> tupleDef <$> parser <*> parser <*> parser
    Struct -> structDef <$> parser <*> parser
    Array -> arrayDef <$> parser <*> parser <*> parser <*> parser

instance Encodable PostDefinition where
  builder (PostDefinition doc args) = builder doc <<>> builder args
instance Decodable PostDefinition where
  parser = PostDefinition <$> parser <*> parser

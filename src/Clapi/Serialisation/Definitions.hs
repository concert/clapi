{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    GADTs
  , LambdaCase
#-}

module Clapi.Serialisation.Definitions where

import Clapi.Serialisation.Base
  (Encodable(..), (<<>>), tdTaggedBuilder, tdTaggedParser)
import Clapi.Serialisation.Path ()
import Clapi.TaggedData (TaggedData, taggedData)
import Clapi.TextSerialisation (ttToText, ttFromText)
import Clapi.TH (btq)
import Clapi.Types.Base (TypeEnumOf(..))
import Clapi.Types.Definitions
  ( Editable(..), MetaType(..)
  , Definition(..), SomeDefinition(..), PostDefinition(..)
  , tupleDef, structDef, arrayDef)
import Clapi.Types.Tree (SomeTreeType(..))

editableTaggedData :: TaggedData Editable Editable
editableTaggedData = taggedData toTag id
  where
    toTag r = case r of
      Editable -> [btq|w|]
      ReadOnly -> [btq|r|]

instance Encodable Editable where
  builder = tdTaggedBuilder editableTaggedData $ const $ return mempty
  parser = tdTaggedParser editableTaggedData return

-- FIXME: do we want to serialise the type to text first?!
instance Encodable SomeTreeType where
  builder (SomeTreeType tt) = builder $ ttToText tt
  parser = parser >>= ttFromText

defTaggedData :: TaggedData MetaType SomeDefinition
defTaggedData = taggedData typeToTag $ \(SomeDefinition d) -> typeEnumOf d
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
  parser = tdTaggedParser defTaggedData $ \mt -> case mt of
    Tuple -> tupleDef <$> parser <*> parser <*> parser
    Struct -> structDef <$> parser <*> parser
    Array -> arrayDef <$> parser <*> parser <*> parser <*> parser

instance Encodable PostDefinition where
  builder (PostDefinition doc args) = builder doc <<>> builder args
  parser = PostDefinition <$> parser <*> parser

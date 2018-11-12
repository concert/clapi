{-# OPTIONS_GHC -Wno-orphans #-}

module Clapi.Serialisation.Definitions where

import Clapi.Serialisation.Base
  (Encodable(..), (<<>>), tdTaggedBuilder, tdTaggedParser)
import Clapi.Serialisation.Path ()
import Clapi.TaggedData (TaggedData, taggedData)
import Clapi.TextSerialisation (ttToText, ttFromText)
import Clapi.TH (btq)
import Clapi.Types.Definitions
  ( Editable(..), MetaType(..), metaType
  , TupleDefinition(..), StructDefinition(..), ArrayDefinition(..)
  , Definition(..), defDispatch, PostDefinition(..))

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

instance Encodable TupleDefinition where
  builder (TupleDefinition doc types interpl) =
    builder doc <<>> builder types <<>> builder interpl
  parser = TupleDefinition <$> parser <*> parser <*> parser

instance Encodable StructDefinition where
  builder (StructDefinition doc tyinfo) = builder doc <<>> builder tyinfo
  parser = StructDefinition <$> parser <*> parser

instance Encodable ArrayDefinition where
  builder (ArrayDefinition doc ptn ctn cl) =
    builder doc <<>> builder ptn <<>> builder ctn <<>> builder cl
  parser = ArrayDefinition <$> parser <*> parser <*> parser <*> parser

defTaggedData :: TaggedData MetaType Definition
defTaggedData = taggedData typeToTag (defDispatch metaType)
  where
    typeToTag mt = case mt of
      Tuple -> [btq|T|]
      Struct -> [btq|S|]
      Array -> [btq|A|]

instance Encodable Definition where
  builder = tdTaggedBuilder defTaggedData $ \def -> case def of
    TupleDef d -> builder d
    StructDef d -> builder d
    ArrayDef d -> builder d
  parser = tdTaggedParser defTaggedData $ \mt -> case mt of
    Tuple -> TupleDef <$> parser
    Struct -> StructDef <$> parser
    Array -> ArrayDef <$> parser

instance Encodable PostDefinition where
  builder (PostDefinition doc args) = builder doc <<>> builder args
  parser = PostDefinition <$> parser <*> parser

{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE QuasiQuotes #-}

module Clapi.Serialisation.SequenceOps () where

import Clapi.TH (btq)
import Clapi.Serialisation.Base (Encodable(..), (<<>>), listBuilder, listParser)
import Clapi.TaggedData (TaggedData, taggedData)
import Clapi.Serialisation.Messages (tdTaggedBuilder, tdTaggedParser)
import Clapi.Types.SequenceOps (SequenceOp(..), ReorderBundle(..))

data SeqOpTy
  = AddAfterTy
  | MoveAfterTy
  | DelElemTy
  deriving (Bounded, Enum)

seqOpTd :: TaggedData SeqOpTy (SequenceOp i)
seqOpTd = taggedData toTag toType
  where
    toTag AddAfterTy = [btq|+|]
    toTag MoveAfterTy = [btq|^|]
    toTag DelElemTy = [btq|-|]
    toType (AddAfter _) = AddAfterTy
    toType (MoveAfter _) = MoveAfterTy
    toType DelElem = DelElemTy

instance Encodable i => Encodable (SequenceOp i) where
    builder = tdTaggedBuilder seqOpTd $ \op -> case op of
        AddAfter mi -> builder mi
        MoveAfter mi -> builder mi
        DelElem -> return mempty
    parser = tdTaggedParser seqOpTd $ \opTy -> case opTy of
        AddAfterTy -> AddAfter <$> parser
        MoveAfterTy -> MoveAfter <$> parser
        DelElemTy -> return DelElem

instance Encodable i => Encodable (ReorderBundle i) where
    builder (ReorderBundle l) = listBuilder buildPair l
      where
        buildPair (i, mi) = builder i <<>> builder mi
    parser = ReorderBundle <$> listParser parsePair
      where
        parsePair = (,) <$> parser <*> parser

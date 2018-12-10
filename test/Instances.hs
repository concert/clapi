{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    GADTs
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , StandaloneDeriving
#-}

module Instances where

import Data.Constraint (Dict(..))
import Data.String (IsString(..))
import Data.Type.Equality (TestEquality(..), (:~:)(..))

import Clapi.PerClientProto (ServerEvent(..))
import Clapi.Types
import Clapi.Types.SequenceOps (SequenceOp(..))
import Clapi.Types.SymbolList (SomeSymbolList(..))
import qualified Clapi.Types.SymbolList as SL
import Clapi.Types.Wire ()


deriving instance Eq PostDefinition
deriving instance Eq (Definition a)

instance TestEquality Definition where
  testEquality (TupleDef {}) (TupleDef {}) = Just Refl
  testEquality (StructDef {}) (StructDef {}) = Just Refl
  testEquality (ArrayDef {}) (ArrayDef {}) = Just Refl
  testEquality _ _ = Nothing

instance Eq SomeDefinition where
  SomeDefinition d1 == SomeDefinition d2 = case testEquality d1 d2 of
    Just Refl -> d1 == d2
    Nothing -> False

deriving instance Eq FrDigest
-- deriving instance Eq FrcRootDigest
-- deriving instance Eq FrcSubDigest
deriving instance Eq FrcUpdateDigest
-- deriving instance Eq FrpDigest

deriving instance Eq TrDigest
deriving instance Eq TrpDigest


deriving instance (Ord ident, Ord a) => Ord (ServerEvent ident a)

deriving instance Ord Attributee
deriving instance IsString Attributee

-- FIXME: TH
getOrd :: WireType a -> Dict (Ord a)
getOrd = \case
  WtTime -> Dict
  WtWord32 -> Dict
  WtWord64 -> Dict
  WtInt32 -> Dict
  WtInt64 -> Dict
  WtFloat -> Dict
  WtDouble -> Dict
  WtString -> Dict
  WtList wt -> case getOrd wt of Dict -> Dict
  WtMaybe wt -> case getOrd wt of Dict -> Dict
  WtPair wt1 wt2 -> case (getOrd wt1, getOrd wt2) of (Dict, Dict) -> Dict

instance Ord (WireValue a) where
  compare (WireValue wt a1) (WireValue _ a2) = case getOrd wt of
    Dict -> compare a1 a2

instance Ord SomeWireValue where
  compare (SomeWireValue wv1) (SomeWireValue wv2) =
    case testEquality wv1 wv2 of
      Just Refl -> compare wv1 wv2
      Nothing -> compare (typeEnumOf wv1) (typeEnumOf wv2)

instance Ord SomeSymbolList where
  compare sl1 sl2 = compare (SL.toStrings_ sl1) (SL.toStrings_ sl2)

instance Ord (TreeType a) where
  compare TtTime TtTime = EQ
  compare (TtEnum sl1) (TtEnum sl2) =
    compare (SomeSymbolList sl1) (SomeSymbolList sl2)
  compare (TtWord32 b1) (TtWord32 b2) = compare b1 b2
  compare (TtWord64 b1) (TtWord64 b2) = compare b1 b2
  compare (TtInt32 b1) (TtInt32 b2) = compare b1 b2
  compare (TtInt64 b1) (TtInt64 b2) = compare b1 b2
  compare (TtFloat b1) (TtFloat b2) = compare b1 b2
  compare (TtDouble b1) (TtDouble b2) = compare b1 b2
  compare (TtString r1) (TtString r2) = compare r1 r2
  compare (TtRef s1) (TtRef s2) = compare s1 s2
  compare (TtList tt1) (TtList tt2) = compare tt1 tt2
  compare (TtSet tt1) (TtSet tt2) = compare tt1 tt2
  compare (TtOrdSet tt1) (TtOrdSet tt2) = compare tt1 tt2
  compare (TtMaybe tt1) (TtMaybe tt2) = compare tt1 tt2
  compare (TtPair tt1a tt1b) (TtPair tt2a tt2b) =
    compare tt1a tt2a <> compare tt1b tt2b

instance Ord SomeTreeType where
  compare (SomeTreeType tt1) (SomeTreeType tt2) =
    case testEquality tt1 tt2 of
      Just Refl -> compare tt2 tt2
      Nothing -> compare (typeEnumOf tt1) (typeEnumOf tt2)

instance Ord (Definition mt) where
  compare (TupleDef doc1 tys1 ilimit1) (TupleDef doc2 tys2 ilimit2) =
    compare doc1 doc2 <> compare tys1 tys2 <> compare ilimit1 ilimit2
  compare (StructDef doc1 tyinfo1) (StructDef doc2 tyinfo2) =
    compare doc1 doc2 <> compare tyinfo1 tyinfo2
  compare (ArrayDef doc1 pt1 ct1 e1) (ArrayDef doc2 pt2 ct2 e2) =
    compare doc1 doc2 <> compare pt1 pt2 <> compare ct1 ct2 <> compare e1 e2

instance Ord SomeDefinition where
  compare (SomeDefinition d1) (SomeDefinition d2) = case testEquality d1 d2 of
    Just Refl -> compare d1 d2
    Nothing -> compare (typeEnumOf d1) (typeEnumOf d2)

deriving instance Ord PostDefinition
deriving instance Ord Editable

deriving instance Ord FrDigest
deriving instance Ord FrcRootDigest
deriving instance Ord FrcSubDigest
deriving instance Ord FrcUpdateDigest
deriving instance Ord FrpDigest
deriving instance Ord FrpErrorDigest

deriving instance Ord CreateOp
deriving instance Ord DataChange
deriving instance Ord a => Ord (DefOp a)
deriving instance Ord TimeSeriesDataOp

deriving instance Ord a => Ord (SequenceOp a)

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    GADTs
  , GeneralizedNewtypeDeriving
  , StandaloneDeriving
#-}

module Instances where

import Data.Constraint (Dict(..))
import Data.String (IsString(..))
import Data.Type.Equality (TestEquality(..), (:~:)(..))

import Clapi.Internal.Valuespace
import Clapi.PerClientProto (ServerEvent(..))
import Clapi.Serialisation (FrDigestType(..))
import Clapi.Tree
import Clapi.Types
import Clapi.Types.SequenceOps (SequenceOp(..))
import Clapi.Valuespace


deriving instance Eq FrDigestType

deriving instance Eq (TrDigest r a)
deriving instance Eq (FrDigest r a)

instance Eq SomeTrDigest where
  SomeTrDigest d1 == SomeTrDigest d2 = go d1 d2
    where
      go (Trpd {}) (Trpd {}) = d1 == d2
      go (Trprd {}) (Trprd {}) = d1 == d2
      go (Trcsd {}) (Trcsd {}) = d1 == d2
      go (Trcud {}) (Trcud {}) = d1 == d2
      go _ _ = False

instance Eq SomeFrDigest where
  SomeFrDigest d1 == SomeFrDigest d2 = go d1 d2
    where
      go (Frpd {}) (Frpd {}) = d1 == d2
      go (Frped {}) (Frped {}) = d1 == d2
      go (Frcrd {}) (Frcrd {}) = d1 == d2
      go (Frcsd {}) (Frcsd {}) = d1 == d2
      go (Frcud {}) (Frcud {}) = d1 == d2
      go _ _ = False


deriving instance Eq RoseTreeNodeType
deriving instance Eq ValidationErr
deriving instance Eq Valuespace


deriving instance (Ord ident, Ord a) => Ord (ServerEvent ident a)

deriving instance Ord Attributee
deriving instance IsString Attributee

instance Ord (WireValue a) where
  compare (WireValue wt a1) (WireValue _ a2) = case getWtOrd wt of
    Dict -> compare a1 a2

instance Ord SomeWireValue where
  compare (SomeWireValue wv1) (SomeWireValue wv2) = case testEquality wv1 wv2 of
    Nothing -> compare (typeEnumOf wv1) (typeEnumOf wv2)
    Just Refl -> compare wv1 wv2

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
  compare (SomeTreeType tt1) (SomeTreeType tt2) = case testEquality tt1 tt2 of
    Nothing -> compare (typeEnumOf tt1) (typeEnumOf tt2)
    Just Refl -> compare tt1 tt2

deriving instance Ord PostDefinition
deriving instance Ord Editability

instance Ord (Definition mt) where
  compare (TupleDef doc1 tys1 ilimit1) (TupleDef doc2 tys2 ilimit2) =
    compare tys1 tys2 <> compare ilimit1 ilimit2 <> compare doc1 doc2
  compare (StructDef doc1 tyinfo1) (StructDef doc2 tyinfo2) =
    compare tyinfo1 tyinfo2 <> compare doc1 doc2
  compare (ArrayDef doc1 pt1 tn1 e1) (ArrayDef doc2 pt2 tn2 e2) =
    compare pt1 pt2 <> compare tn1 tn2 <> compare e1 e2 <> compare doc1 doc2

instance Ord SomeDefinition where
  compare (SomeDefinition d1) (SomeDefinition d2) = case testEquality d1 d2 of
    Just Refl -> compare d1 d2
    Nothing -> compare (typeEnumOf d1) (typeEnumOf d2)


deriving instance Ord CreateOp
deriving instance Ord DataChange
deriving instance Ord a => Ord (DefOp a)
deriving instance Ord TimeSeriesDataOp

deriving instance Ord a => Ord (SequenceOp a)


deriving instance Ord FrDigestType

instance Ord (FrDigest r a) where
  compare (Frpd ns1 dat1 cr1 cops1) (Frpd ns2 dat2 cr2 cops2) =
    compare ns1 ns2 <> compare dat1 dat2 <> compare cr1 cr2
    <> compare cops1 cops2
  compare (Frped errs1) (Frped errs2) = compare errs1 errs2
  compare (Frcrd cops1) (Frcrd cops2) = compare cops1 cops2
  compare (Frcsd errs1 pt1 ty1 dat1) (Frcsd errs2 pt2 ty2 dat2) =
    compare errs1 errs2 <> compare pt1 pt2 <> compare ty1 ty2
    <> compare dat1 dat2
  compare
      (Frcud ns1 pd1 d1 tyas1 dat1 cops1 errs1)
      (Frcud ns2 pd2 d2 tyas2 dat2 cops2 errs2) =
    compare ns1 ns2 <> compare pd1 pd2 <> compare d1 d2 <> compare tyas1 tyas2
    <> compare dat1 dat2 <> compare cops1 cops2 <> compare errs1 errs2

instance Ord SomeFrDigest where
  compare sd1@(SomeFrDigest d1) sd2@(SomeFrDigest d2) = go d1 d2
    where
      go (Frpd {}) (Frpd {}) = compare d1 d2
      go (Frped {}) (Frped {}) = compare d1 d2
      go (Frcrd {}) (Frcrd {}) = compare d1 d2
      go (Frcsd {}) (Frcsd {}) = compare d1 d2
      go (Frcud {}) (Frcud {}) = compare d1 d2
      go _ _ = compare (typeEnumOf sd1) (typeEnumOf sd2)

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    DataKinds
  , FlexibleInstances
  , GADTs
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , OverloadedStrings
  , RankNTypes
  , StandaloneDeriving
  , TemplateHaskell
#-}
module Arbitrary where

import Test.QuickCheck
  ( Arbitrary(..), Gen
  , arbitraryBoundedEnum, choose, elements, listOf, oneof, shuffle)

import Control.Monad (replicateM)
import Data.Constraint (Dict(..))
import Data.Int
import Data.List (inits)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word
import Language.Haskell.TH (Type(ConT))
import System.Random (Random)

import Data.Map.Mol (Mol)
import qualified Data.Map.Mol as Mol
import Data.Map.Mos (Mos)
import qualified Data.Map.Mos as Mos

import Clapi.TextSerialisation (argsOpen, argsClose)
import Clapi.Types hiding (reverse)
import Clapi.Types.SequenceOps (SequenceOp(..))
import qualified Clapi.Types.SymbolList as SL
import Clapi.Types.WireTH (mkGetWtConstraint)


deriving instance Arbitrary a => Arbitrary (Tagged t a)
deriving instance Arbitrary Namespace
deriving instance Arbitrary Placeholder

boundedListOf :: Int -> Int -> Gen a -> Gen [a]
boundedListOf lo hi g = choose (lo, hi) >>= flip replicateM g

smallListOf :: Gen a -> Gen [a]
smallListOf = boundedListOf 0 5

smallListOf1 :: Gen a -> Gen [a]
smallListOf1 = boundedListOf 1 5

smallMapOf :: Ord k => Gen k -> Gen v -> Gen (Map k v)
smallMapOf gk gv = Map.fromList <$> smallListOf ((,) <$> gk <*> gv)

smallMap :: (Ord k, Arbitrary k, Arbitrary v) => Gen (Map k v)
smallMap = smallMapOf arbitrary arbitrary

instance (Ord k, Ord v, Arbitrary k, Arbitrary v) => Arbitrary (Mos k v) where
  arbitrary = Mos.fromMap <$> smallMapOf arbitrary arbitrary
  shrink =
    fmap (Mos.fromList . Map.foldMapWithKey (\k -> fmap (k,) . Set.toList))
    . traverse shrink
    . Mos.unMos

genMol :: Ord k => Gen k -> Gen [v] -> Gen (Mol k v)
genMol k vs = Mol.fromMap <$> smallMapOf k vs

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Mol k v) where
  arbitrary = genMol arbitrary arbitrary
  shrink = fmap Mol.fromMap . traverse shrink . Mol.unMol


assocListOf :: Ord k => Gen k -> Gen v -> Gen (AssocList k v)
assocListOf gk gv = alFromMap <$> smallMapOf gk gv

instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (AssocList a b) where
  arbitrary = assocListOf arbitrary arbitrary
  shrink = fmap unsafeMkAssocList . shrink . unAssocList


arbitraryTextNoNull :: Gen Text
arbitraryTextNoNull = Text.pack . filter (/= '\NUL') <$>
  boundedListOf 0 30 arbitrary

instance Arbitrary Text where
  arbitrary = arbitraryTextNoNull
  shrink = fmap Text.pack . shrink . Text.unpack


name :: Gen Seg
name = fromJust . mkSeg . Text.pack <$> smallListOf1 (elements ['a'..'z'])

instance Arbitrary Seg where
  arbitrary = name

deriving instance Arbitrary Attributee

instance Arbitrary a => Arbitrary (Path' a) where
  arbitrary = Path' <$> smallListOf arbitrary
  shrink (Path' names) = fmap Path' . drop 1 . reverse . inits $ names

instance Arbitrary Editability where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary InterpolationLimit where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Time where
  arbitrary = Time <$> arbitrary <*> arbitrary
  shrink = \case
    Time 0 0 -> []
    Time x y -> [Time x' y' | (x', y') <- shrink (x, y)]

instance Arbitrary Interpolation where
  arbitrary = oneof
      [ return IConstant
      , return ILinear
      , IBezier <$> arbitrary <*> arbitrary]
  shrink = \case
    IBezier _ _ -> [IConstant]
    _ -> []

mkBounds :: Ord a => Maybe a -> Maybe a -> Bounds a
mkBounds a b = either error id $ case (a, b) of
  (Just _, Just _) -> bounds (min a b) (max a b)
  _ -> bounds a b

instance (Ord a, Random a, Arbitrary a) => Arbitrary (Bounds a) where
    arbitrary = mkBounds <$> arbitrary <*> arbitrary
    shrink b = [mkBounds lo hi | (lo, hi) <- shrink (boundsMin b, boundsMax b)]

arbitraryRegex :: Gen Text
arbitraryRegex =
  let
    niceAscii = pure @[] <$> ['!'..'Z'] ++ ['^'..'~']
    escapedArgsDelims = ('\\':) . pure @[] <$> [argsOpen, argsClose]
  in do
    safe <- listOf (elements $ niceAscii ++ escapedArgsDelims)
    delims <- flip replicate "[]" <$> choose (0, 3)
    Text.pack . mconcat <$> shuffle (safe ++ delims)

instance Arbitrary SomeTreeType where
  arbitrary = oneof
    [ return ttTime
    , ttEnum . fmap (Text.unpack . unSeg) <$> arbitrary
    , ttWord32 <$> arbitrary, ttWord64 <$> arbitrary
    , ttInt32 <$> arbitrary, ttInt64 <$> arbitrary
    , ttFloat <$> arbitrary, ttDouble <$> arbitrary
    , ttString <$> arbitraryRegex
    , ttRef <$> arbitrary
    , ttList <$> arbitrary, ttSet <$> arbitrary, ttOrdSet <$> arbitrary
    , ttMaybe <$> arbitrary
    , ttPair <$> arbitrary <*> arbitrary
    ]
  shrink (SomeTreeType tt) = case tt of
    TtTime -> []
    TtEnum sl -> ttEnum <$> shrink (SL.toStrings sl)
    TtWord32 b -> ttWord32 <$> shrink b
    TtWord64 b -> ttWord64 <$> shrink b
    TtInt32 b -> ttInt32 <$> shrink b
    TtInt64 b -> ttInt64 <$> shrink b
    TtFloat b -> ttFloat <$> shrink b
    TtDouble b -> ttDouble <$> shrink b
    TtString r -> case r of
      "" -> []
      _ -> [ttString ""]
    TtRef _ -> []
    TtList tt1 -> ttList <$> shrink (SomeTreeType tt1)
    TtSet tt1 -> ttSet <$> shrink (SomeTreeType tt1)
    TtOrdSet tt1 -> ttOrdSet <$> shrink (SomeTreeType tt1)
    TtMaybe tt1 -> ttMaybe <$> shrink (SomeTreeType tt1)
    TtPair tt1 tt2 ->
      uncurry ttPair <$> shrink (SomeTreeType tt1, SomeTreeType tt2)


-- | An Arbitrary class where we can be more picky about exactly what we
--   generate. Specifically, we want only small lists and text without \NUL
--   characters.
class Arbitrary a => PickyArbitrary a where
  pArbitrary :: Gen a
  pArbitrary = arbitrary

instance PickyArbitrary Time
instance PickyArbitrary Word8
instance PickyArbitrary Word32
instance PickyArbitrary Word64
instance PickyArbitrary Int32
instance PickyArbitrary Int64
instance PickyArbitrary Float
instance PickyArbitrary Double
instance PickyArbitrary Text where
  pArbitrary = arbitraryTextNoNull
instance PickyArbitrary a => PickyArbitrary [a] where
  pArbitrary = smallListOf pArbitrary
instance PickyArbitrary a => PickyArbitrary (Maybe a)
instance (PickyArbitrary a, PickyArbitrary b) => PickyArbitrary (a, b)

instance Arbitrary SomeWireType where
  arbitrary = oneof
    [ return wtTime
    , return wtWord32, return wtWord64
    , return wtInt32, return wtInt64
    , return wtFloat, return wtDouble
    , return wtString
    , wtList <$> arbitrary, wtMaybe <$> arbitrary
    , wtPair <$> arbitrary <*> arbitrary
    ]

mkGetWtConstraint "getArbitrary" $ ConT ''Arbitrary

instance Arbitrary SomeWireValue where
  arbitrary = do
      SomeWireType wt <- arbitrary
      case wt of
        WtList wt1 -> case getArbitrary wt1 of
          Dict -> someWv wt <$> smallListOf arbitrary
        _ -> case getArbitrary wt of
          Dict -> someWv wt <$> arbitrary
  shrink (SomeWireValue (WireValue wt a)) = case getArbitrary wt of
    Dict -> someWv wt <$> shrink a


instance Arbitrary PostDefinition where
  arbitrary = PostDefinition <$> arbitrary <*>
    assocListOf arbitrary (smallListOf arbitrary)

instance Arbitrary (Definition 'Tuple) where
  arbitrary = TupleDef <$> arbitrary <*> arbitrary <*> arbitrary
  shrink (TupleDef d ty ilim) =
    [TupleDef d' ty' ilim' | (d', ty', ilim') <- shrink (d, ty, ilim)]

instance Arbitrary (Definition 'Struct) where
  arbitrary = StructDef <$> arbitrary <*> arbitrary
  shrink (StructDef d tyinfo) =
    [StructDef d' tyinfo' | (d', tyinfo') <- shrink (d, tyinfo)]

instance Arbitrary (Definition 'Array) where
  arbitrary = ArrayDef <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (ArrayDef d ptn tn ed) =
    [ArrayDef d' ptn' tn' ed' | (d', ptn', tn', ed') <- shrink (d, ptn, tn, ed)]

instance Arbitrary SomeDefinition where
  arbitrary = oneof
    [ SomeDefinition <$> arbitrary @(Definition 'Tuple)
    , SomeDefinition <$> arbitrary @(Definition 'Struct)
    , SomeDefinition <$> arbitrary @(Definition 'Array)
    ]
  shrink (SomeDefinition def) = case def of
    TupleDef {} -> SomeDefinition <$> shrink def
    StructDef {} -> SomeDefinition <$> shrink def
    ArrayDef {} -> SomeDefinition <$> shrink def

instance Arbitrary CreateOp where
  arbitrary = OpCreate <$> smallListOf (smallListOf arbitrary) <*> arbitrary
  shrink (OpCreate args after) =
    [OpCreate args' after' | (args', after') <- shrink (args, after)]

instance Arbitrary d => Arbitrary (DefOp d) where
  arbitrary = oneof [OpDefine <$> arbitrary, return OpUndefine]
  shrink = \case
    OpUndefine -> []
    OpDefine def -> OpUndefine : (OpDefine <$> shrink def)

instance Arbitrary TimeSeriesDataOp where
  arbitrary = oneof
    [ OpSet <$> arbitrary <*> smallListOf arbitrary <*> arbitrary
    , return OpRemove]
  shrink = \case
    OpRemove -> []
    OpSet t wvs i ->
      OpRemove : [OpSet t' wvs' i' | (t', wvs', i') <- shrink (t, wvs, i)]

instance Arbitrary DataChange where
  arbitrary = oneof
    [ ConstChange <$> arbitrary <*> arbitrary
    , TimeChange <$> smallMap
    ]

instance Arbitrary i => Arbitrary (SequenceOp i) where
  arbitrary = oneof [SoAfter <$> arbitrary, return SoAbsent]

instance Arbitrary SubOp where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary DataErrorIndex where
  arbitrary = oneof
    [ return GlobalError
    , PathError <$> arbitrary
    , TimePointError <$> arbitrary <*> arbitrary
    ]

instance Arbitrary SubErrorIndex where
  arbitrary = oneof
    [ NamespaceSubError <$> arbitrary
    , PathSubError <$> arbitrary <*> arbitrary
    , TypeSubError <$> arbitrary <*> arbitrary
    , PostTypeSubError <$> arbitrary <*> arbitrary
    ]


creates :: Gen Creates
creates = smallMapOf arbitrary smallMap

contOps :: Arbitrary a => Gen (ContOps a)
contOps = smallMapOf arbitrary smallMap


instance Arbitrary TrpDigest where
  arbitrary = TrpDigest <$> arbitrary <*> smallMap <*> smallMap
    <*> arbitrary <*> contOps <*> arbitrary
  shrink (TrpDigest ns pds ds dat cops _errs) =
    [TrpDigest ns pds' ds' dat' cops' mempty
    | (pds', ds', dat', cops') <- shrink (pds, ds, dat, cops)]

deriving instance Arbitrary TrprDigest

instance Arbitrary TrcSubDigest where
  arbitrary = TrcSubDigest <$> smallMap <*> smallMap <*> smallMap
  shrink (TrcSubDigest pts ts dat) =
    [TrcSubDigest pts' ts' dat' | (pts', ts', dat') <- shrink (pts, ts, dat)]

instance Arbitrary TrcUpdateDigest where
  arbitrary = TrcUpdateDigest <$> arbitrary <*> arbitrary <*> creates
    <*> contOps
  shrink (TrcUpdateDigest ns dat crs cops) =
    [TrcUpdateDigest ns dat' crs' cops'
    | (dat', crs', cops') <- shrink (dat, crs, cops)]


instance Arbitrary FrpDigest where
  arbitrary = FrpDigest <$> arbitrary <*> arbitrary <*> creates <*> contOps
  shrink (FrpDigest ns dat crs cops) =
    [FrpDigest ns dat' crs' cops'
    | (dat', crs', cops') <- shrink (dat, crs, cops)]

instance Arbitrary FrpErrorDigest where
  arbitrary = FrpErrorDigest <$> arbitrary
  shrink (FrpErrorDigest mol) = FrpErrorDigest <$> shrink mol

instance Arbitrary FrcRootDigest where
  arbitrary = FrcRootDigest <$> smallMap
  shrink (FrcRootDigest m) = FrcRootDigest <$> shrink m

instance Arbitrary FrcSubDigest where
  arbitrary = FrcSubDigest <$> smallMapOf arbitrary (smallListOf arbitrary)
    <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (FrcSubDigest e pt t d) =
    [FrcSubDigest e' pt' t' d' | (e', pt', t', d') <- shrink (e, pt, t, d)]

instance Arbitrary FrcUpdateDigest where
  arbitrary = FrcUpdateDigest <$> arbitrary <*> smallMap <*> smallMap
    <*> smallMap <*> arbitrary <*> contOps
    <*> arbitrary
  shrink (FrcUpdateDigest ns pt ty tas d cops _errs) =
    [FrcUpdateDigest ns pt' ty' tas' d' cops' mempty
    | (pt', ty', tas', d', cops') <- shrink (pt, ty, tas, d, cops)]


instance Arbitrary TrDigest where
  arbitrary = oneof
    [ Trpd <$> arbitrary
    , Trprd <$> arbitrary
    , Trcsd <$> arbitrary
    , Trcud <$> arbitrary
    ]
  shrink = \case
    Trpd trpd -> Trpd <$> shrink trpd
    Trprd trprd -> Trprd <$> shrink trprd
    Trcsd trcsd -> Trcsd <$> shrink trcsd
    Trcud trcud -> Trcud <$> shrink trcud

instance Arbitrary FrDigest where
  arbitrary = oneof
    [ Frpd <$> arbitrary
    , Frped <$> arbitrary
    , Frcrd <$> arbitrary
    , Frcsd <$> arbitrary
    , Frcud <$> arbitrary
    ]
  shrink = \case
    Frpd frpd -> Frpd <$> shrink frpd
    Frped frped -> Frped <$> shrink frped
    Frcrd frcrd -> Frcrd <$> shrink frcrd
    Frcsd frcsd -> Frcsd <$> shrink frcsd
    Frcud frcud -> Frcud <$> shrink frcud

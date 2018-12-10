{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    DataKinds
  , FlexibleInstances
  , GADTs
  , GeneralizedNewtypeDeriving
  , RankNTypes
  , StandaloneDeriving
  , TemplateHaskell
#-}
module Arbitrary where

import Test.QuickCheck
  ( Arbitrary(..), Gen
  , arbitraryBoundedEnum, choose, elements, listOf, oneof, shuffle)

import Control.Monad (replicateM)
import Data.Int
import Data.List (inits)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word
import System.Random (Random)

import Data.Map.Mol (Mol)
import qualified Data.Map.Mol as Mol
import Data.Map.Mos (Mos)
import qualified Data.Map.Mos as Mos

import Clapi.TextSerialisation (argsOpen, argsClose)
import Clapi.Types
import Clapi.Types.SequenceOps (SequenceOp(..))
import Clapi.Types.WireTH (mkWithWtProxy)
import Clapi.Util (proxyF, proxyF3)


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

commonKeyPairs :: (Arbitrary k, Arbitrary v) => Gen [(k, v)]
commonKeyPairs = do
    mkPair <- (,) <$> arbitrary
    fmap mkPair <$> smallListOf arbitrary

severalCkps :: (Arbitrary k, Arbitrary v) => Gen [(k, v)]
severalCkps = mconcat <$> smallListOf commonKeyPairs

instance (Ord k, Ord v, Arbitrary k, Arbitrary v) => Arbitrary (Mos k v) where
  arbitrary = Mos.fromList <$> severalCkps

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Mol k v) where
  arbitrary = Mol.fromList <$> severalCkps


assocListOf :: Ord k => Gen k -> Gen v -> Gen (AssocList k v)
assocListOf gk gv = alFromMap <$> smallMapOf gk gv

instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (AssocList a b) where
  arbitrary = assocListOf arbitrary arbitrary


arbitraryTextNoNull :: Gen Text
arbitraryTextNoNull = Text.pack . filter (/= '\NUL') <$>
  boundedListOf 0 30 arbitrary

instance Arbitrary Text where
  arbitrary = arbitraryTextNoNull


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

instance Arbitrary Interpolation where
  arbitrary = oneof
      [ return IConstant
      , return ILinear
      , IBezier <$> arbitrary <*> arbitrary]


instance (Ord a, Random a, Arbitrary a) => Arbitrary (Bounds a) where
    arbitrary = mkBounds <$> arbitrary
      where
        mkBounds (a, b) = either error id $ case (a, b) of
          (Just _, Just _) -> bounds (min a b) (max a b)
          _ -> bounds a b

arbitraryRegex :: Gen Text
arbitraryRegex =
  let
    niceAscii = pure @[] <$> ['!'..'Z'] ++ ['^'..'~']
    escapedArgsDelims = ('\\':) . pure @[] <$> [argsOpen, argsClose]
  in do
    safe <- listOf (elements $ niceAscii ++ escapedArgsDelims)
    delims <- flip replicate "[]" <$> choose (0, 3)
    Text.pack . mconcat <$> shuffle (safe ++ delims)

data TestEnum
  = TestOne | TestTwo | TestThree deriving (Show, Eq, Ord, Enum, Bounded)

instance Arbitrary TreeType where
  arbitrary = oneof
    [ return TtTime
    , return $ ttEnum $ Proxy @TestEnum
    , TtWord32 <$> arbitrary, TtWord64 <$> arbitrary
    , TtInt32 <$> arbitrary, TtInt64 <$> arbitrary
    , TtFloat <$> arbitrary, TtDouble <$> arbitrary
    , TtString <$> arbitraryRegex
    , TtRef <$> arbitrary
    , TtList <$> arbitrary, TtSet <$> arbitrary, TtOrdSet <$> arbitrary
    , TtMaybe <$> arbitrary
    , TtPair <$> arbitrary <*> arbitrary
    ]


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

instance Arbitrary WireType where
  arbitrary = oneof
    [ return WtTime
    , return WtWord32, return WtWord64
    , return WtInt32, return WtInt64
    , return WtFloat, return WtDouble
    , return WtString
    , WtList <$> arbitrary, WtMaybe <$> arbitrary
    , WtPair <$> arbitrary <*> arbitrary
    ]

mkWithWtProxy "withPArbWtProxy" [''PickyArbitrary, ''Wireable]

withPArbWvValue
  :: forall r. WireValue
  -> (forall a. (PickyArbitrary a, Wireable a) => a -> r) -> r
withPArbWvValue wv f = withPArbWtProxy wt g
  where
    wt = wireValueWireType wv
    g :: forall a. (PickyArbitrary a, Wireable a) => Proxy a -> r
    g _ = f $ fromJust $ castWireValue @a wv


instance Arbitrary WireValue where
  arbitrary = do
      wt <- arbitrary @WireType
      withPArbWtProxy wt f
    where
      f :: forall a. (PickyArbitrary a, Wireable a) => Proxy a -> Gen WireValue
      f _ = WireValue <$> pArbitrary @a
  shrink wv = withPArbWvValue wv f
    where
      f :: forall a. (PickyArbitrary a, Wireable a) => a -> [WireValue]
      f a = WireValue <$> shrink a


instance Arbitrary PostDefinition where
  arbitrary = PostDefinition <$> arbitrary <*>
    assocListOf arbitrary (smallListOf arbitrary)

instance Arbitrary (Definition 'Tuple) where
  arbitrary = TupleDef <$> arbitraryTextNoNull <*> arbitrary <*> arbitrary
  shrink (TupleDef d ty ilim) =
    [TupleDef d' ty' ilim' | (d', ty', ilim') <- shrink (d, ty, ilim)]

instance Arbitrary (Definition 'Struct) where
  arbitrary = StructDef <$> arbitraryTextNoNull <*> arbitrary
  shrink (StructDef d tyinfo) =
    [StructDef d' tyinfo' | (d', tyinfo') <- shrink (d, tyinfo)]

instance Arbitrary (Definition 'Array) where
  arbitrary = ArrayDef <$> arbitraryTextNoNull <*> arbitrary <*> arbitrary
    <*> arbitrary
  shrink (ArrayDef d ptn tn ed) =
    [ArrayDef d' ptn' tn' ed' | (d', ptn', tn', ed') <- shrink (d, ptn, tn, ed)]

instance Arbitrary SomeDefinition where
  arbitrary = oneof
    [ SomeDefinition <$> arbitrary @(Definition 'Tuple)
    , SomeDefinition <$> arbitrary @(Definition 'Struct)
    , SomeDefinition <$> arbitrary @(Definition 'Array)
    ]

instance Arbitrary CreateOp where
  arbitrary = OpCreate <$> smallListOf (smallListOf arbitrary) <*> arbitrary

instance Arbitrary d => Arbitrary (DefOp d) where
  arbitrary = oneof [OpDefine <$> arbitrary, return OpUndefine]

instance Arbitrary TimeSeriesDataOp where
  arbitrary = oneof
    [ OpSet <$> arbitrary <*> smallListOf arbitrary <*> arbitrary
    , return OpRemove]

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

deriving instance Arbitrary TrprDigest

instance Arbitrary TrcSubDigest where
  arbitrary = TrcSubDigest <$> smallMap <*> smallMap <*> smallMap

instance Arbitrary TrcUpdateDigest where
  arbitrary = TrcUpdateDigest <$> arbitrary <*> arbitrary <*> creates
    <*> contOps


instance Arbitrary FrpDigest where
  arbitrary = FrpDigest <$> arbitrary <*> arbitrary <*> creates <*> contOps

instance Arbitrary FrpErrorDigest where
  arbitrary = FrpErrorDigest <$> arbitrary

instance Arbitrary FrcRootDigest where
  arbitrary = FrcRootDigest <$> smallMap

instance Arbitrary FrcSubDigest where
  arbitrary = FrcSubDigest <$> smallMapOf arbitrary (smallListOf arbitrary)
    <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary FrcUpdateDigest where
  arbitrary = FrcUpdateDigest <$> arbitrary <*> smallMap <*> smallMap
    <*> smallMap <*> arbitrary <*> contOps
    <*> arbitrary


instance Arbitrary TrDigest where
  arbitrary = oneof
    [ Trpd <$> arbitrary
    , Trprd <$> arbitrary
    , Trcsd <$> arbitrary
    , Trcud <$> arbitrary
    ]

instance Arbitrary FrDigest where
  arbitrary = oneof
    [ Frpd <$> arbitrary
    , Frped <$> arbitrary
    , Frcrd <$> arbitrary
    , Frcsd <$> arbitrary
    , Frcud <$> arbitrary
    ]

{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE
    ExistentialQuantification
  , GeneralizedNewtypeDeriving
  , OverloadedStrings
  , Rank2Types
  , ScopedTypeVariables
  , StandaloneDeriving
  , TemplateHaskell
  , TypeApplications
#-}

module TypesSpec where

import Test.Hspec
import Test.QuickCheck
  ( Arbitrary(..), Gen, property, elements, choose, arbitraryBoundedEnum, oneof
  , listOf, shuffle)

import System.Random (Random)
import Data.Maybe (fromJust)
import Control.Monad (replicateM, liftM2)
import Control.Monad.Fail (MonadFail)
import Data.List (inits)
import Data.Proxy
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8, Word32, Word64)
import Data.Int (Int32, Int64)

import Clapi.TextSerialisation (argsOpen, argsClose)
import Clapi.Types
  ( Time(..), WireValue(..), WireType(..), Wireable, castWireValue, Liberty
  , InterpolationLimit, PostDefinition(..), Definition(..), StructDefinition(..)
  , TupleDefinition(..), ArrayDefinition(..), AssocList, alFromMap
  , wireValueWireType, withWtProxy, Required)
import Clapi.Util (proxyF, proxyF3)

import Clapi.Types.Tree (TreeType(..), Bounds, bounds, ttEnum)
import Clapi.Types.Path (Seg, Path(..), mkSeg, TypeName(..), Namespace(..))
import Clapi.Types.WireTH (mkWithWtProxy)

smallListOf :: Gen a -> Gen [a]
smallListOf g = do
  l <- choose (0, 5)
  replicateM l g

smallListOf1 :: Gen a -> Gen [a]
smallListOf1 g = do
  l <- choose (1, 5)
  replicateM l g

maybeOf :: Gen a -> Gen (Maybe a)
maybeOf g = oneof [return Nothing, Just <$> g]

name :: Gen Seg
name = fromJust . mkSeg . Text.pack <$> smallListOf1 (elements ['a'..'z'])

instance Arbitrary Seg where
  arbitrary = name

instance Arbitrary Path where
  arbitrary = Path <$> smallListOf name
  shrink (Path names) = fmap Path . drop 1 . reverse . inits $ names

instance Arbitrary TypeName where
  arbitrary = TypeName <$> arbitrary <*> arbitrary

instance Arbitrary Liberty where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Required where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary InterpolationLimit where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Time where
  arbitrary = liftM2 Time arbitrary arbitrary

arbitraryTextNoNull :: Gen Text
arbitraryTextNoNull = Text.pack . filter (/= '\NUL') <$> arbitrary

instance Arbitrary Text where
  arbitrary = arbitraryTextNoNull

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
    , return WtWord8, return WtWord32, return WtWord64
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

roundTripWireValue
  :: forall m. MonadFail m => WireValue -> m Bool
roundTripWireValue wv = withWtProxy (wireValueWireType wv) go
  where
    -- This may be the most poncing around with types I've done for the least
    -- value of testing ever!
    go :: forall a. (MonadFail m, Wireable a) => Proxy a -> m Bool
    go _ = (wv ==) . WireValue <$> castWireValue @a wv

spec :: Spec
spec = do
  describe "WireValue" $ do
    it "should survive a round trip via a native Haskell value" $ property $
      either error id . roundTripWireValue

instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (AssocList a b) where
  arbitrary = alFromMap <$> arbitrary

instance Arbitrary PostDefinition where
  arbitrary = PostDefinition <$> arbitrary <*> arbitrary

instance Arbitrary Definition where
    arbitrary =
      do
        oneof
          [ TupleDef <$>
              (TupleDefinition <$> arbitraryTextNoNull <*> arbitrary <*> arbitrary)
          , StructDef <$>
              (StructDefinition <$> arbitraryTextNoNull <*> arbitrary)
          , ArrayDef <$>
              (ArrayDefinition <$> arbitraryTextNoNull <*> arbitrary <*> arbitrary)
          ]

instance (Ord a, Random a, Arbitrary a) => Arbitrary (Bounds a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        either error return $ case (a, b) of
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


data TestEnum = TestOne | TestTwo | TestThree deriving (Show, Eq, Ord, Enum, Bounded)

deriving instance Arbitrary a => Arbitrary (Tagged t a)
deriving instance Arbitrary Namespace

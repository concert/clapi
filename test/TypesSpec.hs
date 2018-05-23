{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}

module TypesSpec where

import Test.Hspec
import Test.QuickCheck
  ( Arbitrary(..), Gen, property, elements, choose, arbitraryBoundedEnum, oneof
  , listOf, shuffle)
import Test.QuickCheck.Instances ()

import System.Random (Random)
import Data.Maybe (fromJust)
import Control.Monad (replicateM, liftM2)
import Control.Monad.Fail (MonadFail)
import Data.List (inits)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8, Word32, Word64)
import Data.Int (Int32, Int64)

import Clapi.TextSerialisation (argsOpen, argsClose)
import Clapi.Types
  ( Time(..), WireValue(..), Wireable, castWireValue, Liberty
  , InterpolationLimit, Definition(..), StructDefinition(..)
  , TupleDefinition(..), ArrayDefinition(..), AssocList, alFromMap
  , wireValueWireType, withWtProxy)

import Clapi.Types.Tree (TreeType(..), Bounds, bounds, ttEnum)
import Clapi.Types.Path (Seg, Path(..), mkSeg, TypeName(..))

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

instance Arbitrary InterpolationLimit where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Time where
  arbitrary = liftM2 Time arbitrary arbitrary

arbitraryTextNoNull :: Gen Text
arbitraryTextNoNull = Text.filter (/= '\NUL') <$> arbitrary @Text

instance Arbitrary WireValue where
  arbitrary = pickConcrete
    where
      pickConcrete = do
        concT <- arbitraryBoundedEnum
        depth <- choose (0, 2)
        case concT of
          WcTime -> contain depth (arbitrary @Time)
          WcWord8 -> contain depth (arbitrary @Word8)
          WcWord32 -> contain depth (arbitrary @Word32)
          WcWord64 -> contain depth (arbitrary @Word64)
          WcInt32 -> contain depth (arbitrary @Int32)
          WcInt64 -> contain depth (arbitrary @Int64)
          WcFloat -> contain depth (arbitrary @Float)
          WcDouble -> contain depth (arbitrary @Double)
          WcString -> contain depth arbitraryTextNoNull
      contain :: Wireable a => Int -> Gen a -> Gen WireValue
      contain 0 g = WireValue <$> g
      contain depth g = oneof
        [ contain (depth - 1) $ smallListOf g
        , contain (depth - 1) $ maybeOf g
        ]
  shrink wv = let (concT, contTs) = unpackWireType $ wireValueWireType wv in
      case concT of
          WcTime -> lThing contTs (Proxy @Time)
          WcWord8 -> lThing contTs (Proxy @Word8)
          WcWord32 -> lThing contTs (Proxy @Word32)
          WcWord64 -> lThing contTs (Proxy @Word64)
          WcInt32 -> lThing contTs (Proxy @Int32)
          WcInt64 -> lThing contTs (Proxy @Int64)
          WcFloat -> lThing contTs (Proxy @Float)
          WcDouble -> lThing contTs (Proxy @Double)
          WcString -> lThing contTs (Proxy @Text)
    where
      lThing
        :: forall a. (Wireable a, Arbitrary a) => [WireContainerType]
        -> Proxy a -> [WireValue]
      lThing [] _ = fmap WireValue $ shrink @a $ fromJust $ castWireValue wv
      lThing (contT:contTs) _ = case contT of
        WcList -> lThing contTs (Proxy @[a])
        WcMaybe -> lThing contTs (Proxy @(Maybe a))

roundTripWireValue
  :: forall m. MonadFail m => WireValue -> m Bool
roundTripWireValue wv = withWireTypeProxy go $ wireValueWireType wv
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

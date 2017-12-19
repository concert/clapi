{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes, ScopedTypeVariables #-}
module TypesSpec where

import Test.Hspec
import Test.QuickCheck
  ( Arbitrary(..), Gen, property, oneof, elements, choose, listOf
  , counterexample)

import Control.Monad (replicateM, liftM2)
import Control.Monad.Fail (MonadFail)
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16, Word32, Word64)
import Data.Int (Int32, Int64)

import Clapi.Path (Name, Path(..))
import Clapi.Types (
    CanFail, DataUpdateMessage(UMsgSet), RequestBundle(..), Time(..),
    ClapiValue(..), Interpolation(..), fromClapiValue, toClapiValue,
    Enumerated)
import Clapi.Serialisation (encode, decode)

smallListOf :: Gen a -> Gen [a]
smallListOf g = do
  l <- choose (0, 5)
  replicateM l g

smallListOf1 :: Gen a -> Gen [a]
smallListOf1 g = do
  l <- choose (1, 5)
  replicateM l g

name :: Gen Name
name = Text.pack <$> smallListOf1 (elements ['a'..'z'])

instance Arbitrary Path where
  arbitrary = Path <$> listOf name

instance Arbitrary Time where
  arbitrary = liftM2 Time arbitrary arbitrary

instance Arbitrary Interpolation where
  arbitrary = oneof
      [ return IConstant
      , return ILinear
      , IBezier <$> arbitrary <*> arbitrary]

data TestEnum = One | Two | Three deriving (Bounded, Eq, Show, Ord, Enum)

instance Arbitrary TestEnum where
  arbitrary = elements [minBound..]

instance Arbitrary ClapiValue where
  arbitrary = oneof gens
    where
      gens =
        [ arbTime, arbEnum, arbWord32, arbWord64, arbInt32, arbInt64, arbFloat
        , arbDouble, arbString, arbList
        ]
      arbTime = ClTime <$> arbitrary
      arbEnum = ClEnum . fromIntegral . fromEnum <$> (arbitrary :: Gen TestEnum)
      arbWord32 = ClWord32 <$> arbitrary
      arbWord64 = ClWord64 <$> arbitrary
      arbInt32 = ClInt32 <$> arbitrary
      arbInt64 = ClInt64 <$> arbitrary
      arbFloat = ClFloat <$> arbitrary
      arbDouble = ClDouble <$> arbitrary
      arbString = ClString . Text.pack <$> arbitrary
      arbList = fmap ClList $ elements gens >>= smallListOf

instance Arbitrary DataUpdateMessage where
  arbitrary = UMsgSet
    <$> arbitrary  -- Path
    <*> arbitrary  -- Time
    <*> listOf arbitrary  -- Args
    <*> arbitrary  -- Interpolation
    <*> arbitrary  -- Attributee
    <*> arbitrary  -- Site

roundTripClapiValue :: forall m. MonadFail m => ClapiValue -> m ClapiValue
roundTripClapiValue cl@(ClTime _) =
    toClapiValue <$> (fromClapiValue cl :: m Time)
roundTripClapiValue cl@(ClEnum _) =
    toClapiValue <$> (fromClapiValue cl :: m (Enumerated TestEnum))
roundTripClapiValue cl@(ClWord32 _) =
    toClapiValue <$> (fromClapiValue cl :: m Word32)
roundTripClapiValue cl@(ClWord64 _) =
    toClapiValue <$> (fromClapiValue cl :: m Word64)
roundTripClapiValue cl@(ClInt32 _) =
    toClapiValue <$> (fromClapiValue cl :: m Int32)
roundTripClapiValue cl@(ClInt64 _) =
    toClapiValue <$> (fromClapiValue cl :: m Int64)
roundTripClapiValue cl@(ClFloat _) =
    toClapiValue <$> (fromClapiValue cl :: m Float)
roundTripClapiValue cl@(ClDouble _) =
    toClapiValue <$> (fromClapiValue cl :: m Double)
roundTripClapiValue cl@(ClString _) =
    toClapiValue <$> (fromClapiValue cl :: m Text)
roundTripClapiValue (ClList cvs) =
    -- FIXME: this is a bit of a lie, as we done round-trip via Clapiable for
    -- lists...
    ClList <$> mapM roundTripClapiValue cvs

spec :: Spec
spec = do
  describe "ClapiValue" $ do
    it "should survive a round trip via a native Haskell value" $ property $
      \(x :: ClapiValue) -> roundTripClapiValue x == Just x

    describe "serialisation" $ do
      it "should fail to encode overly long string" $
        let
          n = fromIntegral (maxBound :: Word16)
          longStr = replicate (n + 1) 'a'
        in
          encode longStr `shouldSatisfy` isLeft

      it "should survive a round trip via binary" $ property $
        \(ms :: [DataUpdateMessage]) -> let
            b = RequestBundle [] ms
            result = encode b >>= decode :: CanFail RequestBundle
          in
            counterexample (show result) $ Right b == result

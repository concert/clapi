{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module TextSerialisationSpec where

import Data.Proxy

import Test.Hspec
import Test.QuickCheck (Arbitrary(..), property, oneof)
import Test.QuickCheck.Instances ()
import System.Random (Random)

import Clapi.TextSerialisation (ttFromText, ttToText)
import Clapi.Types.Tree (TreeConcreteType(..), tcEnum, TreeContainerType(..), TreeType(..), Bounds, bounds)
import Clapi.Types ()
import TypesSpec ()

data TestEnum = TestOne | TestTwo | TestThree deriving (Show, Eq, Ord, Enum, Bounded)

instance (Ord a, Random a, Arbitrary a) => Arbitrary (Bounds a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        either error return $ case (a, b) of
            (Just _, Just _) -> bounds (min a b) (max a b)
            _ -> bounds a b

instance Arbitrary TreeConcreteType where
    arbitrary = oneof [
        arbTime, arbEnum, arbWord32, arbWord64, arbInt32, arbInt64, arbFloat,
        arbDouble, arbString, arbRef, arbValidatorDesc]
      where
        arbTime = return TcTime
        arbEnum = return $ tcEnum (Proxy :: Proxy TestEnum)
        arbWord32 = TcWord32 <$> arbitrary
        arbWord64 = TcWord64 <$> arbitrary
        arbInt32 = TcInt32 <$> arbitrary
        arbInt64 = TcInt64 <$> arbitrary
        arbFloat = TcFloat <$> arbitrary
        arbDouble = TcDouble <$> arbitrary
        arbString = TcString <$> arbitrary
        arbRef = TcRef <$> arbitrary
        arbValidatorDesc = return TcValidatorDesc

instance Arbitrary TreeContainerType where
    arbitrary = oneof [arbList, arbSet, arbOrdSet]
      where
        arbList = TcList <$> arbitrary
        arbSet = TcSet <$> arbitrary
        arbOrdSet = TcOrdSet <$> arbitrary

instance Arbitrary TreeType where
    arbitrary = oneof [TtConc <$> arbitrary, TtCont <$> arbitrary]

spec :: Spec
spec = do
    describe "Tree type descriptions" $ do
        it "should survive a round trip to text" $ property $
            \tt -> either error id (ttFromText (ttToText tt)) `shouldBe` tt
        it "should fail to deserialise nonsense" $
          ttFromText "this is not a type" `shouldBe` Nothing

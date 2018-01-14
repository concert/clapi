{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}

module TypesSpec where

import Test.Hspec
import Test.QuickCheck
  (Arbitrary(..), Gen, property, elements, choose, arbitraryBoundedEnum)
import Test.QuickCheck.Instances ()

import Data.Maybe (fromJust)
import Control.Monad (replicateM, liftM2)
import Control.Monad.Fail (MonadFail)
import Data.List (inits)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8, Word32, Word64)
import Data.Int (Int32, Int64)

import Clapi.Serialisation
  ( WireContainerType(..), WireConcreteType(..), wireValueWireType
  , withWireTypeProxy, unpackWireType)
import Clapi.Types (Time(..), WireValue(..), Wireable, castWireValue)
import Clapi.Types.Path (Seg, Path(..), mkSeg)

smallListOf :: Gen a -> Gen [a]
smallListOf g = do
  l <- choose (0, 5)
  replicateM l g

smallListOf1 :: Gen a -> Gen [a]
smallListOf1 g = do
  l <- choose (1, 5)
  replicateM l g

name :: Gen Seg
name = fromJust . mkSeg . Text.pack <$> smallListOf1 (elements ['a'..'z'])

instance Arbitrary Seg where
  arbitrary = name

instance Arbitrary Path where
  arbitrary = Path <$> smallListOf name
  shrink (Path names) = fmap Path . drop 1 . reverse . inits $ names

instance Arbitrary Time where
  arbitrary = liftM2 Time arbitrary arbitrary

arbitraryTextNoNull :: Gen Text
arbitraryTextNoNull = Text.pack . filter (/= '\NUL') <$> arbitrary @String

instance Arbitrary WireValue where
  arbitrary = pickConcrete
    where
      pickConcrete = do
        concT <- arbitraryBoundedEnum
        depth <- choose (0, 2)
        blargh depth concT
      blargh depth concT = case concT of
          WcTime -> listify depth (arbitrary @Time)
          WcWord8 -> listify depth (arbitrary @Word8)
          WcWord32 -> listify depth (arbitrary @Word32)
          WcWord64 -> listify depth (arbitrary @Word64)
          WcInt32 -> listify depth (arbitrary @Int32)
          WcInt64 -> listify depth (arbitrary @Int64)
          WcFloat -> listify depth (arbitrary @Float)
          WcDouble -> listify depth (arbitrary @Double)
          WcString -> listify depth arbitraryTextNoNull
      listify ::
        forall a. Wireable a => Int -> Gen a -> Gen WireValue
      listify 0 g = WireValue <$> g
      listify depth g = listify (depth - 1) (smallListOf g)
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
      lThing :: forall a. (Wireable a, Arbitrary a) => [WireContainerType] -> Proxy a -> [WireValue]
      lThing [] _ = fmap WireValue $ shrink @a $ fromJust $ castWireValue wv
      lThing (contT:contTs) _ = case contT of
        WcList -> lThing contTs (Proxy @[a])

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

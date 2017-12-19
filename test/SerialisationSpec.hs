{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SerialisationSpec where

import Test.Hspec
import Test.QuickCheck
  (property, counterexample, Arbitrary(..), Gen, oneof)
import Test.QuickCheck.Instances ()

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.Word (Word16)

import Clapi.Path (Path(..), Name)
import Clapi.Types
  ( CanFail, Time, Attributee, Site, ClapiValue, RequestBundle
  , Interpolation(..), SubMessage(..), RequestBundle(..), DataUpdateMessage(..))
import Clapi.Serialisation (encode, decode)

import TypesSpec (smallListOf, name) -- Incl. Arbitrary instances of ClapiValue


instance Arbitrary Interpolation where
  arbitrary = oneof
      [ return IConstant
      , return ILinear
      , IBezier <$> arbitrary <*> arbitrary]

instance Arbitrary SubMessage where
  arbitrary = oneof
      [ UMsgSubscribe <$> arbitrary
      , UMsgUnsubscribe <$> arbitrary]

instance Arbitrary RequestBundle where
  arbitrary = RequestBundle <$> arbitrary <*> arbitrary
  shrink (RequestBundle subs msgs) =
    [RequestBundle subs' msgs' | (subs', msgs') <- shrink (subs, msgs)]

instance Arbitrary DataUpdateMessage where
  arbitrary = oneof
    [ UMsgAdd
      <$> (arbitrary :: Gen Path)
      <*> (arbitrary :: Gen Time)
      <*> (smallListOf arbitrary :: Gen [ClapiValue])
      <*> (arbitrary :: Gen Interpolation)
      <*> (arbitrary :: Gen (Maybe Attributee))
      <*> (arbitrary :: Gen (Maybe Site))
    , UMsgSet
      <$> (arbitrary :: Gen Path)
      <*> (arbitrary :: Gen Time)
      <*> (smallListOf arbitrary :: Gen [ClapiValue])
      <*> (arbitrary :: Gen Interpolation)
      <*> (arbitrary :: Gen (Maybe Attributee))
      <*> (arbitrary :: Gen (Maybe Site))
    , UMsgRemove
      <$> (arbitrary :: Gen Path)
      <*> (arbitrary :: Gen Time)
      <*> (arbitrary :: Gen (Maybe Attributee))
      <*> (arbitrary :: Gen (Maybe Site))
    , UMsgClear
      <$> (arbitrary :: Gen Path)
      <*> (arbitrary :: Gen Time)
      <*> (arbitrary :: Gen (Maybe Attributee))
      <*> (arbitrary :: Gen (Maybe Site))
    , UMsgSetChildren
      <$> (arbitrary :: Gen Path)
      <*> (smallListOf name :: Gen [Name])
      <*> (arbitrary :: Gen (Maybe Attributee))
    ]
  shrink (UMsgSet (Path []) _ [] _ Nothing Nothing) = []
  shrink (UMsgSet p t vs i a s) =
    [UMsgSet p' t vs' i a' s' | (p', vs', a', s') <- shrink (p, vs, a, s)]
  shrink _ = []


showBytes :: ByteString -> String
showBytes = show . BS.unpack


spec :: Spec
spec =
    describe "serialisation" $ do
      it "should fail to encode overly long string" $
        let
          n = fromIntegral (maxBound :: Word16)
          longStr = replicate (n + 1) 'a'
        in
          encode longStr `shouldSatisfy` isLeft

      it "should survive a round trip via binary" $ property $
        \(b :: RequestBundle) -> let
            mbs = encode b
            result = mbs >>= decode :: CanFail RequestBundle
          in
            counterexample (show mbs) $
            counterexample (show $ showBytes <$> mbs) $
            counterexample (show result) $
            Right b == result

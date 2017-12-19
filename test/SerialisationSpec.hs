{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SerialisationSpec where

import Test.Hspec
import Test.QuickCheck
  (property, Property, counterexample, Arbitrary(..), Gen, oneof)
import Test.QuickCheck.Instances ()

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.Word (Word16)

import Clapi.Path (Path(..), Name)
import Clapi.Types
  ( CanFail, Time, Attributee, Site, ClapiValue, RequestBundle
  , Interpolation(..), SubMessage(..), DataUpdateMessage(..)
  , TreeUpdateMessage(..), RequestBundle(..)  , UpdateBundle(..)
  , OwnerRequestBundle(..), FromRelayBundle(..)  , ToRelayBundle(..)
  , UMsgError(..))
import Clapi.Serialisation (Serialisable, encode, decode)

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
  shrink (UMsgAdd (Path []) _ [] _ Nothing Nothing) = []
  shrink (UMsgAdd p t vs i a s) =
    [UMsgSet p' t vs' i a' s' | (p', vs', a', s') <- shrink (p, vs, a, s)]
  shrink (UMsgSet (Path []) _ [] _ Nothing Nothing) = []
  shrink (UMsgSet p t vs i a s) =
    [UMsgSet p' t vs' i a' s' | (p', vs', a', s') <- shrink (p, vs, a, s)]
  shrink (UMsgRemove (Path []) _ Nothing Nothing) = []
  shrink (UMsgRemove p t a s) =
    [UMsgRemove p' t a' s' | (p', a', s') <- shrink (p, a, s)]
  shrink (UMsgClear p t a s) =
    [UMsgClear p' t a' s' | (p', a', s') <- shrink (p, a, s)]
  shrink (UMsgSetChildren (Path []) [] Nothing) = []
  shrink (UMsgSetChildren p ns a) =
    [UMsgSetChildren p' ns' a' | (p', ns', a') <- shrink (p, ns, a)]

instance Arbitrary TreeUpdateMessage where
  arbitrary = oneof
    [ UMsgAssignType
      <$> (arbitrary :: Gen Path)
      <*> (arbitrary :: Gen Path)
    , UMsgDelete <$> (arbitrary :: Gen Path)]

instance Arbitrary UMsgError where
  arbitrary = UMsgError <$> arbitrary <*> arbitrary

instance Arbitrary RequestBundle where
  arbitrary = RequestBundle <$> smallListOf arbitrary <*> smallListOf arbitrary
  shrink (RequestBundle subs msgs) =
    [RequestBundle subs' msgs' | (subs', msgs') <- shrink (subs, msgs)]

instance Arbitrary UpdateBundle where
  arbitrary = UpdateBundle <$> smallListOf arbitrary <*> smallListOf arbitrary

instance Arbitrary OwnerRequestBundle where
  arbitrary =
    OwnerRequestBundle <$> smallListOf arbitrary <*> smallListOf arbitrary

instance Arbitrary FromRelayBundle where
  arbitrary = oneof [FRBClient <$> arbitrary, FRBOwner <$> arbitrary]

instance Arbitrary ToRelayBundle where
  arbitrary = oneof [TRBClient <$> arbitrary, TRBOwner <$> arbitrary]


showBytes :: ByteString -> String
showBytes = show . BS.unpack


propBundleRoundTrip
  :: forall bundle. (Show bundle, Eq bundle, Serialisable bundle)
  => bundle -> Property
propBundleRoundTrip b = let
    mbs = encode b
    result = mbs >>= decode :: CanFail bundle
  in
    counterexample (show mbs) $
    counterexample (show $ showBytes <$> mbs) $
    counterexample (show result) $
    Right b == result


spec :: Spec
spec = do
  it "should fail to encode overly long string" $
    let
      n = fromIntegral (maxBound :: Word16)
      longStr = replicate (n + 1) 'a'
    in
      encode longStr `shouldSatisfy` isLeft

  describe "ToRelayBundle" $
    it "should survive a round trip via binary" $ property $
      \(b :: ToRelayBundle) -> propBundleRoundTrip b

  describe "FromRelayBundle" $
    it "should survive a round trip via binary" $ property $
      \(b :: FromRelayBundle) -> propBundleRoundTrip b

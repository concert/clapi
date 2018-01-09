{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SerialisationSpec where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Test.Hspec
import Test.QuickCheck
  (property, Property, counterexample, Arbitrary(..), Gen, oneof)
import Test.QuickCheck.Instances ()

import Blaze.ByteString.Builder (toByteString)
import Data.Attoparsec.ByteString (parseOnly, endOfInput)

import Clapi.Path (Path(..), Seg)
import Clapi.Types
  ( Time, Attributee, Site, WireValue, RequestBundle
  , Interpolation(..), SubMessage(..), DataUpdateMessage(..)
  , TreeUpdateMessage(..), RequestBundle(..)  , UpdateBundle(..)
  , OwnerRequestBundle(..), FromRelayBundle(..)  , ToRelayBundle(..)
  , MsgError(..))
import Clapi.Serialisation (Encodable(..))

 -- Incl. Arbitrary instances of WireValue:
import TypesSpec (smallListOf, name, arbitraryTextNoNull)

encode :: (MonadFail m, Encodable a) => a -> m ByteString
encode x = toByteString <$> builder x

decode :: (MonadFail m, Encodable a) => ByteString -> m a
decode = either fail return . parseOnly (parser <* endOfInput)

instance Arbitrary Interpolation where
  arbitrary = oneof
      [ return IConstant
      , return ILinear
      , IBezier <$> arbitrary <*> arbitrary]

instance Arbitrary SubMessage where
  arbitrary = oneof
      [ MsgSubscribe <$> arbitrary
      , MsgUnsubscribe <$> arbitrary]

genAttributee :: Gen (Maybe Attributee)
genAttributee = oneof [return Nothing, Just <$> arbitraryTextNoNull]

genSite :: Gen (Maybe Site)
genSite = genAttributee

instance Arbitrary DataUpdateMessage where
  arbitrary = oneof
    [ MsgAdd
      <$> (arbitrary :: Gen Path)
      <*> (arbitrary :: Gen Time)
      <*> (smallListOf arbitrary :: Gen [WireValue])
      <*> (arbitrary :: Gen Interpolation)
      <*> genAttributee
      <*> genSite
    , MsgSet
      <$> (arbitrary :: Gen Path)
      <*> (arbitrary :: Gen Time)
      <*> (smallListOf arbitrary :: Gen [WireValue])
      <*> (arbitrary :: Gen Interpolation)
      <*> genAttributee
      <*> genSite
    , MsgRemove
      <$> (arbitrary :: Gen Path)
      <*> (arbitrary :: Gen Time)
      <*> genAttributee
      <*> genSite
    , MsgClear
      <$> (arbitrary :: Gen Path)
      <*> (arbitrary :: Gen Time)
      <*> genAttributee
      <*> genSite
    , MsgSetChildren
      <$> (arbitrary :: Gen Path)
      <*> (smallListOf name :: Gen [Seg])
      <*> genAttributee
    ]
  shrink (MsgAdd (Path []) _ [] _ Nothing Nothing) = []
  shrink (MsgAdd p t vs i a s) =
    [MsgSet p' t vs' i a' s' | (p', vs', a', s') <- shrink (p, vs, a, s)]
  shrink (MsgSet (Path []) _ [] _ Nothing Nothing) = []
  shrink (MsgSet p t vs i a s) =
    [MsgSet p' t vs' i a' s' | (p', vs', a', s') <- shrink (p, vs, a, s)]
  shrink (MsgRemove (Path []) _ Nothing Nothing) = []
  shrink (MsgRemove p t a s) =
    [MsgRemove p' t a' s' | (p', a', s') <- shrink (p, a, s)]
  shrink (MsgClear p t a s) =
    [MsgClear p' t a' s' | (p', a', s') <- shrink (p, a, s)]
  shrink (MsgSetChildren (Path []) [] Nothing) = []
  shrink (MsgSetChildren p ns a) =
    [MsgSetChildren p' ns' a' | (p', ns', a') <- shrink (p, ns, a)]

instance Arbitrary TreeUpdateMessage where
  arbitrary = oneof
    [ MsgAssignType
      <$> (arbitrary :: Gen Path)
      <*> (arbitrary :: Gen Path)
    , MsgDelete <$> (arbitrary :: Gen Path)]

instance Arbitrary MsgError where
  arbitrary = MsgError <$> arbitrary <*> arbitraryTextNoNull

instance Arbitrary RequestBundle where
  arbitrary = RequestBundle <$> smallListOf arbitrary <*> smallListOf arbitrary
  shrink (RequestBundle subs msgs) =
    [RequestBundle subs' msgs' | (subs', msgs') <- shrink (subs, msgs)]

instance Arbitrary UpdateBundle where
  arbitrary = UpdateBundle <$> smallListOf arbitrary <*> smallListOf arbitrary
  shrink (UpdateBundle errs msgs) =
    [UpdateBundle errs' msgs' | (errs', msgs') <- shrink (errs, msgs)]

instance Arbitrary OwnerRequestBundle where
  arbitrary =
    OwnerRequestBundle <$> smallListOf arbitrary <*> smallListOf arbitrary

instance Arbitrary FromRelayBundle where
  arbitrary = oneof [FRBClient <$> arbitrary, FRBOwner <$> arbitrary]

instance Arbitrary ToRelayBundle where
  arbitrary = oneof [TRBClient <$> arbitrary, TRBOwner <$> arbitrary]
  shrink (TRBClient b) = TRBClient <$> shrink b
  shrink (TRBOwner b) = TRBOwner <$> shrink b


showBytes :: ByteString -> String
showBytes = show . BS.unpack


propBundleRoundTrip
  :: forall bundle. (Show bundle, Eq bundle, Encodable bundle)
  => bundle -> Property
propBundleRoundTrip b = let
    mbs = encode b
    result = mbs >>= decode :: Either String bundle
  in
    counterexample (show mbs) $
    counterexample (show $ showBytes <$> mbs) $
    counterexample (show result) $
    Right b == result


spec :: Spec
spec = do
  describe "ToRelayBundle" $
    it "should survive a round trip via binary" $ property $
      \(b :: ToRelayBundle) -> propBundleRoundTrip b

  describe "FromRelayBundle" $
    it "should survive a round trip via binary" $ property $
      \(b :: FromRelayBundle) -> propBundleRoundTrip b

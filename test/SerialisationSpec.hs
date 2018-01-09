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
  , UMsgError(..))
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
      [ UMsgSubscribe <$> arbitrary
      , UMsgUnsubscribe <$> arbitrary]

genAttributee :: Gen (Maybe Attributee)
genAttributee = oneof [return Nothing, Just <$> arbitraryTextNoNull]

genSite :: Gen (Maybe Site)
genSite = genAttributee

instance Arbitrary DataUpdateMessage where
  arbitrary = oneof
    [ UMsgAdd
      <$> (arbitrary :: Gen Path)
      <*> (arbitrary :: Gen Time)
      <*> (smallListOf arbitrary :: Gen [WireValue])
      <*> (arbitrary :: Gen Interpolation)
      <*> genAttributee
      <*> genSite
    , UMsgSet
      <$> (arbitrary :: Gen Path)
      <*> (arbitrary :: Gen Time)
      <*> (smallListOf arbitrary :: Gen [WireValue])
      <*> (arbitrary :: Gen Interpolation)
      <*> genAttributee
      <*> genSite
    , UMsgRemove
      <$> (arbitrary :: Gen Path)
      <*> (arbitrary :: Gen Time)
      <*> genAttributee
      <*> genSite
    , UMsgClear
      <$> (arbitrary :: Gen Path)
      <*> (arbitrary :: Gen Time)
      <*> genAttributee
      <*> genSite
    , UMsgSetChildren
      <$> (arbitrary :: Gen Path)
      <*> (smallListOf name :: Gen [Seg])
      <*> genAttributee
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
  arbitrary = UMsgError <$> arbitrary <*> arbitraryTextNoNull

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

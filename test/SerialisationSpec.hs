{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}

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

import Clapi.Types
  ( Time, Attributee, Site, WireValue
  , Interpolation(..), SubMessage(..), DataUpdateMessage(..), TypeMessage(..)
  , MsgError(..), TpId, DefMessage(..), ContainerUpdateMessage(..)
  , ToRelayClientBundle(..), ToRelayProviderBundle(..)
  , FromRelayClientBundle(..), FromRelayProviderBundle(..)
  , FromRelayProviderErrorBundle(..), ToRelayProviderRelinquish(..)
  , ToRelayBundle(..), FromRelayBundle(..)
  , ErrorIndex(..))
import Clapi.Types.Path (Path(..), Seg, pattern Root)
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

instance Arbitrary a => Arbitrary (DefMessage a) where
  arbitrary = oneof
    [ MsgDefine <$> arbitrary <*> arbitrary
    , MsgUndefine <$> arbitrary
    ]

instance Arbitrary SubMessage where
  arbitrary = oneof
      [ MsgTypeSubscribe <$> arbitrary
      , MsgSubscribe <$> arbitrary
      , MsgTypeUnsubscribe <$> arbitrary
      , MsgUnsubscribe <$> arbitrary]

instance Arbitrary TypeMessage where
  arbitrary = MsgAssignType <$> arbitrary <*> arbitrary <*> arbitrary

genAttributee :: Gen (Maybe Attributee)
genAttributee = oneof [return Nothing, Just <$> arbitraryTextNoNull]

instance Arbitrary DataUpdateMessage where
  arbitrary = oneof
    [ MsgConstSet
      <$> (arbitrary :: Gen Path)
      <*> (smallListOf arbitrary :: Gen [WireValue])
      <*> genAttributee
    , MsgSet
      <$> (arbitrary :: Gen Path)
      <*> (arbitrary :: Gen TpId)
      <*> (arbitrary :: Gen Time)
      <*> (smallListOf arbitrary :: Gen [WireValue])
      <*> (arbitrary :: Gen Interpolation)
      <*> genAttributee
    , MsgRemove
      <$> (arbitrary :: Gen Path)
      <*> (arbitrary :: Gen TpId)
      <*> genAttributee
    ]
  shrink (MsgConstSet Root [] Nothing) = []
  shrink (MsgConstSet p vs a) =
    [MsgConstSet p' vs' a' | (p', vs', a') <- shrink (p, vs, a)]
  shrink (MsgSet (Path []) _ _ [] _ Nothing) = []
  shrink (MsgSet p tpid t vs i a) =
    [MsgSet p' tpid t vs' i a' | (p', vs', a') <- shrink (p, vs, a)]
  shrink (MsgRemove (Path []) _ Nothing) = []
  shrink (MsgRemove p t a) = [MsgRemove p' t a' | (p', a') <- shrink (p, a)]


instance Arbitrary ContainerUpdateMessage where
  arbitrary = undefined

instance Arbitrary a => Arbitrary (ErrorIndex a) where
  arbitrary = oneof
    [ return GlobalError
    , PathError <$> arbitrary
    , TimePointError <$> arbitrary <*> arbitrary
    , TypeNameError <$> arbitrary
    ]

instance Arbitrary a => Arbitrary (MsgError a) where
  arbitrary = MsgError <$> arbitrary <*> arbitraryTextNoNull


instance Arbitrary ToRelayProviderBundle where
  arbitrary = ToRelayProviderBundle <$> arbitrary <*> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary

instance Arbitrary FromRelayProviderBundle where
  arbitrary = FromRelayProviderBundle <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ToRelayProviderRelinquish where
  arbitrary = ToRelayProviderRelinquish <$> arbitrary

instance Arbitrary FromRelayProviderErrorBundle where
  arbitrary = FromRelayProviderErrorBundle <$> arbitrary <*> arbitrary

instance Arbitrary ToRelayClientBundle where
  arbitrary = ToRelayClientBundle <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary FromRelayClientBundle where
  arbitrary = FromRelayClientBundle <$> arbitrary <*> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary ToRelayBundle where
  arbitrary = oneof
    [ Trpb <$> arbitrary
    , Trpr <$> arbitrary
    , Trcb <$> arbitrary
    ]

instance Arbitrary FromRelayBundle where
  arbitrary = oneof
    [ Frpb <$> arbitrary
    , Frpeb <$> arbitrary
    , Frcb <$> arbitrary
    ]


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

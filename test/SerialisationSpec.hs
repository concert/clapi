{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module SerialisationSpec where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Test.Hspec
import Test.QuickCheck
  (property, Property, counterexample, Arbitrary(..), Gen, oneof)

import Blaze.ByteString.Builder (toByteString)
import Data.Attoparsec.ByteString (parseOnly, endOfInput)

import Clapi.Types
  ( Time, Attributee, WireValue
  , Interpolation(..), SubMessage(..), DataUpdateMessage(..), TypeMessage(..)
  , DataErrorMessage(..), TpId, DefMessage(..)
  , ToClientContainerUpdateMessage(..)
  , ToProviderContainerUpdateMessage(..)
  , ToRelayClientSubBundle(..), ToRelayClientUpdateBundle(..)
  , ToRelayProviderBundle(..)
  , FromRelayClientRootBundle(..), FromRelayClientSubBundle(..)
  , FromRelayClientUpdateBundle(..)
  , FromRelayProviderBundle(..)
  , FromRelayProviderErrorBundle(..), ToRelayProviderRelinquish(..)
  , ToRelayBundle(..), FromRelayBundle(..)
  , DataErrorIndex(..), SubErrorMessage(..), SubErrorIndex(..))
import Clapi.Types.Path (Path, pattern Root)
import Clapi.Serialisation (Encodable(..))

-- Incl. Arbitrary instances of WireValue, Namespace, Placeholder:
import TypesSpec (smallListOf, arbitraryTextNoNull)

encode :: (MonadFail m, Encodable a) => a -> m ByteString
encode x = toByteString <$> builder x

decode :: (MonadFail m, Encodable a) => ByteString -> m a
decode = either fail return . parseOnly (parser <* endOfInput)

instance Arbitrary Interpolation where
  arbitrary = oneof
      [ return IConstant
      , return ILinear
      , IBezier <$> arbitrary <*> arbitrary]

instance (Arbitrary ident, Arbitrary def)
    => Arbitrary (DefMessage ident def) where
  arbitrary = oneof
    [ MsgDefine <$> arbitrary <*> arbitrary
    , MsgUndefine <$> arbitrary
    ]

instance Arbitrary SubMessage where
  arbitrary = oneof
      [ MsgPostTypeSubscribe <$> arbitrary
      , MsgTypeSubscribe <$> arbitrary
      , MsgSubscribe <$> arbitrary
      , MsgPostTypeUnsubscribe <$> arbitrary
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
  shrink (MsgSet Root _ _ [] _ Nothing) = []
  shrink (MsgSet p tpid t vs i a) =
    [MsgSet p' tpid t vs' i a' | (p', vs', a') <- shrink (p, vs, a)]
  shrink (MsgRemove Root _ Nothing) = []
  shrink (MsgRemove p t a) = [MsgRemove p' t a' | (p', a') <- shrink (p, a)]


instance Arbitrary ToProviderContainerUpdateMessage where
  arbitrary = oneof
    [ TpcumCreateAfter <$> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary
    , TpcumMoveAfter <$> arbitrary <*> arbitrary <*> genAttributee
    , TpcumAbsent <$> arbitrary <*> genAttributee
    ]

instance Arbitrary ToClientContainerUpdateMessage where
  arbitrary = oneof
    [ TccumPresentAfter <$> arbitrary <*> arbitrary <*> genAttributee
    , TccumAbsent <$> arbitrary <*> genAttributee
    ]

instance Arbitrary DataErrorIndex where
  arbitrary = oneof
    [ return GlobalError
    , PathError <$> arbitrary
    , TimePointError <$> arbitrary <*> arbitrary
    ]

instance Arbitrary SubErrorIndex where
  arbitrary = oneof
    [ PathSubError <$> arbitrary
    , TypeSubError <$> arbitrary
    , PostTypeSubError <$> arbitrary
    ]

instance Arbitrary DataErrorMessage where
  arbitrary = MsgDataError <$> arbitrary <*> arbitraryTextNoNull

instance Arbitrary SubErrorMessage where
  arbitrary = MsgSubError <$> arbitrary <*> arbitraryTextNoNull

instance Arbitrary ToRelayProviderBundle where
  arbitrary = ToRelayProviderBundle
    <$> arbitrary <*> smallListOf arbitrary <*> smallListOf arbitrary
    <*> smallListOf arbitrary <*> smallListOf arbitrary
    <*> smallListOf arbitrary
  shrink (ToRelayProviderBundle n e pt t d c) =
    [ToRelayProviderBundle n e' pt' t' d' c' |
       (e', pt', t', d', c') <- shrink (e, pt, t, d, c)]

instance Arbitrary FromRelayProviderBundle where
  arbitrary = FromRelayProviderBundle <$> arbitrary
    <*> smallListOf arbitrary <*> smallListOf arbitrary

instance Arbitrary ToRelayProviderRelinquish where
  arbitrary = ToRelayProviderRelinquish <$> arbitrary

instance Arbitrary FromRelayProviderErrorBundle where
  arbitrary = FromRelayProviderErrorBundle <$> smallListOf arbitrary

instance Arbitrary ToRelayClientSubBundle where
  arbitrary = ToRelayClientSubBundle <$> arbitrary

instance Arbitrary ToRelayClientUpdateBundle where
  arbitrary = ToRelayClientUpdateBundle <$> arbitrary
    <*> smallListOf arbitrary <*> smallListOf arbitrary
  shrink (ToRelayClientUpdateBundle n d c) =
    [ToRelayClientUpdateBundle n d' c' | (d', c') <- shrink (d, c)]

instance Arbitrary FromRelayClientSubBundle where
  arbitrary = FromRelayClientSubBundle
    <$> smallListOf arbitrary <*> smallListOf arbitrary
    <*> smallListOf arbitrary <*> smallListOf arbitrary
  shrink (FromRelayClientSubBundle e ptu tu du) =
    [FromRelayClientSubBundle e' ptu' tu' du'
    | (e', ptu', tu', du')
    <- shrink (e, ptu, tu, du)]

instance Arbitrary FromRelayClientUpdateBundle where
  arbitrary = FromRelayClientUpdateBundle <$> arbitrary
    <*> smallListOf arbitrary <*> smallListOf arbitrary
    <*> smallListOf arbitrary <*> smallListOf arbitrary
    <*> smallListOf arbitrary <*> smallListOf arbitrary
  shrink (FromRelayClientUpdateBundle ns e postDefs defs tas dd c) =
    [FromRelayClientUpdateBundle ns e' postDefs' defs' tas' dd' c'
    | (e', postDefs', defs', tas', dd', c')
    <- shrink (e, postDefs, defs, tas, dd, c)]

deriving instance Arbitrary FromRelayClientRootBundle

instance Arbitrary ToRelayBundle where
  arbitrary = oneof
    [ Trpb <$> arbitrary
    , Trpr <$> arbitrary
    , Trcsb <$> arbitrary
    , Trcub <$> arbitrary
    ]
  shrink (Trpb b) = Trpb <$> shrink b
  shrink (Trpr b) = Trpr <$> shrink b
  shrink (Trcsb b) = Trcsb <$> shrink b
  shrink (Trcub b) = Trcub <$> shrink b

instance Arbitrary FromRelayBundle where
  arbitrary = oneof
    [ Frpb <$> arbitrary
    , Frpeb <$> arbitrary
    , Frcrb <$> arbitrary
    , Frcsb <$> arbitrary
    , Frcub <$> arbitrary
    ]
  shrink (Frpb b) = Frpb <$> shrink b
  shrink (Frpeb b) = Frpeb <$> shrink b
  shrink (Frcrb b) = Frcrb <$> shrink b
  shrink (Frcsb b) = Frcsb <$> shrink b
  shrink (Frcub b) = Frcub <$> shrink b


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

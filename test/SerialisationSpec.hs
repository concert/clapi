{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , StandaloneDeriving
#-}

module SerialisationSpec where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Test.Hspec
import Test.QuickCheck (property, Property, counterexample)

import Blaze.ByteString.Builder (toByteString)
import Data.Attoparsec.ByteString (parseOnly, endOfInput)

import Clapi.Types (TrDigest(..), FrDigest(..))
import Clapi.Serialisation (Encodable(..))

import Arbitrary ()


encode :: (MonadFail m, Encodable a) => a -> m ByteString
encode x = toByteString <$> builder x

decode :: (MonadFail m, Encodable a) => ByteString -> m a
decode = either fail return . parseOnly (parser <* endOfInput)


showBytes :: ByteString -> String
showBytes = show . BS.unpack


propDigestRoundTrip
  :: forall digest. (Show digest, Eq digest, Encodable digest)
  => digest -> Property
propDigestRoundTrip d = let
    mbs = encode d
    result = mbs >>= decode :: Either String digest
  in
    counterexample (show mbs) $
    counterexample (show $ showBytes <$> mbs) $
    counterexample (show result) $
    Right d == result


spec :: Spec
spec = do
  describe "ToRelayDigest" $
    it "should survive a round trip via binary" $ property $
      \(b :: TrDigest) -> propDigestRoundTrip b

  describe "FromRelayDigest" $
    it "should survive a round trip via binary" $ property $
      \(b :: FrDigest) -> propDigestRoundTrip b

{-# LANGUAGE
    OverloadedStrings
#-}

module SerialisationProtocolSpec where
import Test.Hspec

import Control.Monad (forever)
import Control.Monad.Trans (lift)
import qualified Data.ByteString as B

import qualified Data.Map.Mol as Mol

import Instances ()

import Clapi.Types (FrDigest(..), FrpErrorDigest(..), DataErrorIndex(..))
import Clapi.Protocol
  ((<<->), sendRev, sendFwd, waitThenFwdOnly, waitThenRevOnly, runEffect)
import Clapi.Serialisation ()
import Clapi.SerialisationProtocol (serialiser)

spec :: Spec
spec = it "Packetised round trip" $
    runEffect $ chunkyEcho <<-> serialiser <<-> test
  where
    msg :: FrDigest
    msg = Frped $ FrpErrorDigest $ Mol.singleton GlobalError "part of test"
    chunkyEcho = forever $ waitThenRevOnly $ \c ->
      let (c0, c1) = B.splitAt (B.length c `div` 2) c in
        sendFwd c0 >> sendFwd c1
    test = do
        sendRev msg
        waitThenFwdOnly $ \m -> lift $ m `shouldBe` msg

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    ExistentialQuantification
  , GeneralizedNewtypeDeriving
  , Rank2Types
  , StandaloneDeriving
  , TemplateHaskell
#-}

module TypesSpec where

import Test.Hspec
import Test.QuickCheck (property)

import Control.Monad.Fail (MonadFail)
import Data.Proxy

import Clapi.Types
  (WireValue(..), Wireable, castWireValue, wireValueWireType, withWtProxy)

import Arbitrary ()

roundTripWireValue
  :: forall m. MonadFail m => WireValue -> m Bool
roundTripWireValue wv = withWtProxy (wireValueWireType wv) go
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

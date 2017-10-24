{-# LANGUAGE OverloadedStrings #-}
module TestTextSerialisation (tests) where
import qualified Data.Map as Map
import Test.HUnit (assertEqual)
import Test.Framework.Providers.HUnit (testCase)
import Blaze.ByteString.Builder (toByteString)

import Clapi.Types (Time(..), ClapiValue(..))
import Clapi.TextSerialisation (encode, decode)

tests = [
   testCase "basic text encode" testBasicEncode,
   testCase "basic text decode" testBasicDecode
   ]

timeseries = Map.fromList [(Time 0 0, [ClBool True, ClInt32 3, ClString "marimba"])]
encoded _ = "Bis\n0:0 T 3 \"marimba\""

testBasicEncode = assertEqual "encode result" (encoded undefined) $
    toByteString . encode $ timeseries

testBasicDecode = assertEqual "decode result" (Right timeseries) $ decode (encoded undefined)

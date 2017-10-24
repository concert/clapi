{-# LANGUAGE OverloadedStrings #-}
module TestTextSerialisation (tests) where
import qualified Data.Map as Map
import Test.HUnit (assertEqual)
import Test.Framework.Providers.HUnit (testCase)
import Blaze.ByteString.Builder (toByteString)

import Clapi.Types (Time(..), ClapiValue(..))
import Clapi.TextSerialisation (encode)

tests = [
   testCase "basic text encode" testBasicEncode
   ]

testBasicEncode = assertEqual "encode result" "Bis\n0:0 T 3 \"marimba\"" $
    toByteString . encode $ Map.fromList [(Time 0 0, [ClBool True, ClInt32 3, ClString "marimba"])]

{-# LANGUAGE OverloadedStrings #-}
module TestTextSerialisation (tests) where
import Test.HUnit (assertEqual)
import Test.Framework.Providers.HUnit (testCase)
import Blaze.ByteString.Builder (toByteString)

import Clapi.Types (Time(..), ClapiValue(..), Message(..), Interpolation(..))
import Clapi.TextSerialisation (encode, decode)

tests = [
   testCase "basic text encode" testBasicEncode,
   testCase "basic text decode" testBasicDecode
   ]

path = ["te", "st"]
msgs = [
    MsgAdd path (Time 0 0) [ClBool True, ClInt32 3, ClString "marimba"] IConstant Nothing Nothing
  , MsgSet path (Time 0 0) [ClBool False, ClInt32 4, ClString "xylophone"] IConstant Nothing Nothing
  , MsgRemove path (Time 0 0) (Just "Bob") Nothing]
encoded _ = "Bis\na 0:0 T 3 \"marimba\" C \"\"\ns 0:0 F 4 \"xylophone\" C \"\"\nr 0:0 \"Bob\"\nend"

testBasicEncode = assertEqual "encode result" (encoded undefined) $
    toByteString . encode $ msgs

testBasicDecode = assertEqual "decode result" (Right msgs) $ decode path (encoded undefined)

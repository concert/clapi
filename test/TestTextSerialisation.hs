{-# LANGUAGE OverloadedStrings #-}
module TestTextSerialisation (tests) where
import Data.String (IsString)
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
    MsgAdd path (Time 0 0) [ClBool True, ClEnum 0, ClInt32 3, ClString "marimba"] IConstant Nothing Nothing
  , MsgSet path (Time 0 0) [ClBool False, ClEnum 0, ClInt32 4, ClString "xylophone"] IConstant Nothing Nothing
  , MsgRemove path (Time 0 0) (Just "Bob") Nothing]

encoded :: IsString a => a
encoded = "Beis\na 0:0 T 0 3 \"marimba\" C \"\"\ns 0:0 F 0 4 \"xylophone\" C \"\"\nr 0:0 \"Bob\""

testBasicEncode = assertEqual "encode result" encoded $
    toByteString . encode $ msgs

testBasicDecode = assertEqual "decode result" (Right msgs) $ decode path encoded

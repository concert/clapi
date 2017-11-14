{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module TestTextSerialisation (tests) where
import Data.String (IsString)
import Test.HUnit (assertEqual)
import Test.Framework.Providers.HUnit (testCase)
import Blaze.ByteString.Builder (toByteString)

import Clapi.Types (Time(..), ClapiValue(..), DataUpdateMessage(..), Interpolation(..))
import Clapi.TextSerialisation (encode, decode)
import Clapi.PathQ

tests = [
   testCase "basic text encode" testBasicEncode,
   testCase "basic text decode" testBasicDecode
   ]

path = [pathq|/te/st|]
msgs = [
    UMsgAdd path (Time 0 0) [ClEnum 0, ClInt32 3, ClString "marimba"] IConstant Nothing Nothing
  , UMsgSet path (Time 0 0) [ClEnum 0, ClInt32 4, ClString "xylophone"] IConstant Nothing Nothing
  , UMsgRemove path (Time 0 0) (Just "Bob") Nothing]

encoded :: IsString a => a
encoded = "eis\na 0:0 0 3 \"marimba\" C \"\"\ns 0:0 0 4 \"xylophone\" C \"\"\nr 0:0 \"Bob\""

testBasicEncode = assertEqual "encode result" encoded $
    toByteString . encode $ msgs

testBasicDecode = assertEqual "decode result" (Right msgs) $ decode path encoded

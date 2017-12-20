{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module TextSerialisationSpec (spec) where
import Test.Hspec

import Data.String (IsString)
import Blaze.ByteString.Builder (toByteString)

import Clapi.Types (Time(..), ClapiValue(..), DataUpdateMessage(..), Interpolation(..))
import Clapi.TextSerialisation (encode, decode)
import Clapi.PathQ

path = [pathq|/te/st|]
msgs = [
    UMsgAdd path (Time 0 0) [ClEnum 0, ClInt32 3, ClString "marimba"] IConstant Nothing Nothing
  , UMsgSet path (Time 0 0) [ClEnum 0, ClInt32 4, ClString "xylophone"] IConstant Nothing Nothing
  , UMsgRemove path (Time 0 0) (Just "Bob") Nothing]

encoded :: IsString a => a
encoded = "eis\na 0:0 0 3 \"marimba\" C \"\"\ns 0:0 0 4 \"xylophone\" C \"\"\nr 0:0 \"Bob\""

spec = do
    it "Encodes" $ toByteString (encode msgs) `shouldBe` encoded
    it "Decodes" $ decode path encoded `shouldBe` Right msgs

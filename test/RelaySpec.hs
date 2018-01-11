{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
module RelaySpec where
import Data.List (intersperse, (\\))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Word
import Test.Hspec

import Clapi.Path (pattern Root, toText)
import Clapi.PathQ
import Clapi.Types (
    DataUpdateMessage(..), TreeUpdateMessage(..), OwnerUpdateMessage(..),
    Interpolation(IConstant), Time(..), WireValue(..))
import Clapi.Valuespace (getType, baseValuespace, Liberty(..))
import Clapi.Relay (handleMessages)
import Clapi.NamespaceTracker (Request(..), Response(..))

spec :: Spec
spec = describe "Relay" $ do
    it "Extends root struct" $ updates `shouldBe` updates
  where
    inMsgs = [
        Left $ MsgAssignType [pathq|/foo/type|] [pathq|/api/types/base/struct|],
        Right $ MsgAdd
            [pathq|/foo/type|]
            z
            [t "atypical", lot ["type"], lop [[pathq|/api/types/base/struct|]], low [cannot]]
            IConstant
            Nothing
            Nothing,
        Left $ MsgAssignType [pathq|/foo|] [pathq|/foo/type|]]
    expectedMsgs = inMsgs ++ [Right $ MsgSet (tp Root) z extendedV IConstant Nothing Nothing]
    z = Time 0 0
    extendedV = [t "auto-generated container", lot ["foo", "api"], lop [[pathq|/foo/type|], tp [pathq|/api|]], low [cannot, cannot]]
    t = WireValue @Text
    lot = WireValue @[Text]
    lop = lot . fmap toText
    low = WireValue @[Word8]
    tp p = fromJust $ getType baseValuespace p
    (GoodOwnerResponse updates, vs') = handleMessages baseValuespace $ OwnerRequest inMsgs
    cannot = fromIntegral $ fromEnum Cannot

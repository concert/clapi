{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
module RelaySpec where
import Data.List (intersperse, (\\))
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Test.Hspec

import Clapi.Path (pattern Root, toText)
import Clapi.PathQ
import Clapi.Types (
    DataUpdateMessage(..), TreeUpdateMessage(..), OwnerUpdateMessage(..),
    Interpolation(IConstant), Time(..), ClapiValue(..), Enumerated(..),
    toClapiValue)
import Clapi.Valuespace (getType, baseValuespace, Liberty(..))
import Clapi.Relay (handleMessages)
import Clapi.NamespaceTracker (Request(..), Response(..))

spec :: Spec
spec = describe "Relay" $ do
    it "Extends root struct" $ updates `shouldBe` updates
  where
    inMsgs = [
        Left $ UMsgAssignType [pathq|/foo/type|] [pathq|/api/types/base/struct|],
        Right $ UMsgAdd
            [pathq|/foo/type|]
            z
            [s "atypical", los ["type"], los ["/api/types/base/struct"], ClList [cannot]]
            IConstant
            Nothing
            Nothing,
        Left $ UMsgAssignType [pathq|/foo|] [pathq|/foo/type|]]
    expectedMsgs = inMsgs ++ [Right $ UMsgSet (tp Root) z extendedV IConstant Nothing Nothing]
    z = Time 0 0
    extendedV = [s "auto-generated container", los ["foo", "api"], lop [[pathq|/foo/type|], tp [pathq|/api|]], ClList [cannot, cannot]]
    s = ClString
    los = ClList . (map s)
    lop ps = los $ map toText ps
    tp p = fromJust $ getType baseValuespace p
    (GoodOwnerResponse updates, vs') = handleMessages baseValuespace $ OwnerRequest inMsgs
    cannot = toClapiValue $ Enumerated Cannot

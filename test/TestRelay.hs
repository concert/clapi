{-# LANGUAGE OverloadedStrings #-}
module TestRelay where
import Data.List (intersperse, (\\))
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual)

import Path.Parsing (toString)
import Clapi.Types (Message(..), Interpolation(IConstant), Time(..), ClapiValue(..), Enumerated(..), toClapiValue)
import Clapi.Valuespace (getType, baseValuespace, Liberty(..))
import Clapi.Relay (handleMessages)
import Clapi.NamespaceTracker (RoutableMessage(..), Ownership(..))

tests = [
    testCase "test root struct extending" testRootStructExtend
    ]

newtype NicerShow = NicerShow [Message] deriving (Eq)

instance Show NicerShow where
    show (NicerShow m) = "[\n" ++ (concat $ intersperse "\n" $ map show m) ++ "\n]"

assertMsgsInclude a b = assertEqual ("missing items in " ++ (show $ NicerShow b)) (NicerShow []) (NicerShow $ a \\ b)

testRootStructExtend = assertMsgsInclude expectedMsgs rMsgs
  where
    inMsgs = [
        MsgAssignType ["foo", "type"] ["api", "types", "base", "struct"],
        MsgAdd
            ["foo", "type"]
            z
            [cannot, s "atypical", los ["type"], los ["/api/types/base/struct/"], ClList [cannot]]
            IConstant
            Nothing
            Nothing,
        MsgAssignType ["foo"] ["foo", "type"]]
    expectedMsgs = inMsgs ++ [MsgSet (tp []) z extendedV IConstant Nothing Nothing]
    z = Time 0 0
    extendedV = [cannot, s "auto-generated container", los ["foo", "api"], lop [["foo", "type"], tp ["api"]], ClList [cannot, cannot]]
    s = ClString . T.pack
    los = ClList . (map s)
    lop ps = los $ map toString ps
    tp p = fromJust $ getType baseValuespace p
    om = map (\m -> (Owner, m))
    (ms', vs') = handleMessages baseValuespace $ om inMsgs
    rMsgs = map rmMsg ms'
    cannot = toClapiValue $ Enumerated Cannot

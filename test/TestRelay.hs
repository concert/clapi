{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module TestRelay where
import Data.List (intersperse, (\\))
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual)

import Path.Parsing (toText)
import Clapi.Path (root)
import Clapi.PathQ
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
        MsgAssignType [pathq|/foo/type|] [pathq|/api/types/base/struct|],
        MsgAdd
            [pathq|/foo/type|]
            z
            [cannot, s "atypical", los ["type"], los ["/api/types/base/struct/"], ClList [cannot]]
            IConstant
            Nothing
            Nothing,
        MsgAssignType [pathq|/foo|] [pathq|/foo/type|]]
    expectedMsgs = inMsgs ++ [MsgSet (tp root) z extendedV IConstant Nothing Nothing]
    z = Time 0 0
    extendedV = [cannot, s "auto-generated container", los ["foo", "api"], lop [[pathq|/foo/type|], tp [pathq|/api|]], ClList [cannot, cannot]]
    s = ClString
    los = ClList . (map s)
    lop ps = los $ map toText ps
    tp p = fromJust $ getType baseValuespace p
    om = map (\m -> (Owner, m))
    (ms', vs') = handleMessages baseValuespace $ om inMsgs
    rMsgs = map rmMsg ms'
    cannot = toClapiValue $ Enumerated Cannot

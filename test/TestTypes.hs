{-# LANGUAGE OverloadedStrings #-}
module TestTypes where

import Test.HUnit ((@=?), assertEqual)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Maybe (isNothing)
import Data.Word (Word16)
import qualified Data.Map.Strict as Map

import Helpers (assertFailed)
import Clapi.Path (root, up)
import Clapi.Types (
    CanFail, Message(..), Time(..), ClapiValue(..), ClapiMethod(..),
    Interpolation(..), fromClapiValue, toClapiValue)
import Clapi.Serialisation (encode, decode)


tests = [
    testProperty "roundtrip ClapiValue" testClapiValueConversionRoundTrip,
    testCase "roundtrip message" testBinarySerialisationRoundTrip,
    testCase "string length" testEncodeTooLongString,
    testCase "up" testUp
    ]


-- FIXME: we should define a QuickCheck.Arbitrary instance for ClapiValue and
-- use it to generate better random values
testClapiValueConversionRoundTrip :: [Float] -> Bool
testClapiValueConversionRoundTrip f =
    (fromClapiValue . toClapiValue) f == Just f

testBinarySerialisationRoundTrip =
    Right bundle @=? result where
        bundle = [message, message]
        message = MsgSet -- FIXME: might want to do property testing for this
            ["hello", "world"]
            (Time 0 0)
            nestedArgList
            ILinear
            Nothing
            (Just "home")
        argList = [
            CBool True, CBool False, CTime (Time 4 2),
            CEnum 12, CWord32 32, CWord64 64, CInt32 (-32), CInt64 (-64),
            CFloat 15.1, CDouble 13.2, CString "Greetings Planet"]
        nestedArgList = (CList argList) : argList

        result = encode bundle >>= decode :: CanFail [Message]

testEncodeTooLongString =
    assertFailed "Long string not detected" $ encode longStr
    where
      n = fromIntegral $ (maxBound :: Word16)
      longStr = replicate (n + 1) 'a'

testUp =
  do
    assertEqual "root up failed" root $ up root
    assertEqual "normal up failed" ["a"] $ up ["a", "b"]

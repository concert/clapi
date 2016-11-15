{-# LANGUAGE OverloadedStrings #-}
module TestTypes where

import Test.HUnit ((@=?))
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck (quickCheck)

import Data.Word (Word16)
import qualified Data.Map.Strict as Map

import Util (assertFailed)
import Types (
    Time(..), ClapiValue(..), ClapiMessage(..), ClapiMethod(..), ClapiBundle,
    fromClapiValue, toClapiValue)

import Serialisation (encode, decode)


tests = [
    testCase "roundtrip ClapiValue" testClapiValueConversionRoundTrip,
    testCase "roundtrip message" testBinarySerialisationRoundTrip,
    testCase "string length" testEncodeTooLongString
    ]


-- FIXME: we should define a QuickCheck.Arbitrary instance for ClapiValue and
-- use it to generate better random values
testClapiValueConversionRoundTrip = quickCheck propRoundTrip
  where
    propRoundTrip :: [Float] -> Bool
    propRoundTrip f = (fromClapiValue . toClapiValue) f == f

testBinarySerialisationRoundTrip =
    Right bundle @=? result where
        bundle = [message, message]
        message = CMessage
            ["hello", "world"]
            Error
            nestedArgList
            (zip [[c] | c <- ['a'..'z']] nestedArgList)
        argList = [
            CBool True, CBool False, CTime (Time 4 2),
            CWord32 32, CWord64 64, CInt32 (-32), CInt64 (-64), CFloat 15.1,
            CDouble 13.2, CString "Greetings Planet"]
        nestedArgList = (CList argList) : argList

        result = encode bundle >>= decode :: Either String ClapiBundle

testEncodeTooLongString =
    assertFailed "Long string not detected" $ encode longStr
    where
      n = fromIntegral $ (maxBound :: Word16)
      longStr = replicate (n + 1) 'a'

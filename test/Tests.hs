{-# LANGUAGE OverloadedStrings #-}
module Tests where

import Data.Word (Word16)
import Test.HUnit ((@=?), assertBool)

import Types (
    Time(..), ClapiValue(..), ClapiMessage(..), ClapiMethod(..), ClapiBundle)
import Serialisation (encode, decode)


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
    assertBool "Long string not detected" (didFail result)
    where
      didFail (Left _) = True
      didFail (Right _) = False
      n = fromIntegral $ (maxBound :: Word16)
      longStr = replicate (n + 1) 'a'
      result = encode longStr

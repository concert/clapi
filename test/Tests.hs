{-# LANGUAGE OverloadedStrings #-}
module Tests where

import Test.HUnit ((@=?))

import Types (ClapiValue(..), ClapiMessage(..), ClapiMethod(..), ClapiBundle)
import Serialisation (encode, decode)


-- testRoundTripClapiMethod =


testBinarySerialisationRoundTrip =
    Right bundle @=? result where
        bundle = [message, message]
        message = CMessage
            ["hello", "world"]
            Error
            nestedArgList
            (zip [[c] | c <- ['a'..'z']] nestedArgList)
        argList = [
            CNil, CBool True, CBool False, CTime 4 2, CWord32 32, CWord64 64,
            CInt32 (-32), CInt64 (-64), CFloat 15.1, CDouble 13.2,
            CString "Greetings Planet"]
        nestedArgList = (CList argList) : argList

        result = encode bundle >>= decode :: Either String ClapiBundle

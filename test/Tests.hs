module Tests where

import Test.HUnit ((@=?))

import Serialisation (ClapiValue(..), ClapiMessage(..), encode, decode)
import Path (BasePath(..), Method(..))


-- testDisplayBasePath =
--   "/hello/world/" @=? display (BasePath ["hello", "world"])

-- testParseBasePath =
--   Right (BasePath ["hello", "world"]) @=? parse "/hello/world/"

testBinarySerialisationRoundTrip =
    Right bundle @=? (decode . encode $ bundle) where
        bundle = [message, message]
        message = CMessage
            (BasePath ["hello", "world"])
            Error
            nestedArgList
            (zip [[c] | c <- ['a'..'z']] nestedArgList)
        argList = [
            CNil, CBool True, CBool False, CTime 4 2, CWord32 32, CWord64 64,
            CInt32 (-32), CInt64 (-64), CFloat 15.1, CDouble 13.2,
            CString "Greetings Planet"]
        nestedArgList = (CList argList) : argList

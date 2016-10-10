{-# LANGUAGE DeriveDataTypeable #-}
module Lib
    (
        someFunc,
    ) where

import Path (BasePath(..), Method(..))
import Blaze.ByteString.Builder (toByteString)
import Serialisation (ClapiValue(..), ClapiMessage(..), encode, decode, someBytes)
import Data.Data (Typeable, Data, dataTypeOf, dataTypeConstrs, Constr, toConstr)
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map

-- someFunc :: IO ()
-- someFunc = putStrLn $ show $ dataTypeConstrs $ dataTypeOf OscNil

myMessage = CMessage
    (BasePath ["hello", "world"])
    Error
    [CString "Greetings Planet"]
    [("foo", CInt32 1)]

myPacket = [myMessage, myMessage]

-- someFunc :: IO()
-- someFunc = putStrLn . show . toByteString $ encode myPacket


someFunc :: IO()
someFunc = putStrLn . show $ ((decode someBytes) :: Either String Int)

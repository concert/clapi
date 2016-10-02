{-# LANGUAGE DeriveDataTypeable #-}
module Lib
    (
        someFunc,
    ) where

import Path (Path, toOsc)
import Blaze.ByteString.Builder (toByteString)
import Serialisation (ClapiValue(..), encode)
import Data.Data (Typeable, Data, dataTypeOf, dataTypeConstrs, Constr, toConstr)
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map

-- someFunc :: IO ()
-- someFunc = putStrLn $ show $ dataTypeConstrs $ dataTypeOf OscNil

someFunc :: IO()
someFunc = putStrLn . show . toByteString $ encode[CString "hello world"]


-- data OscValue = OscNil | OscBool Bool | OscTimeTag Int32 Int32 |
--     OscInt32 Int32 | OscInt64 Int64 | OscFloat Float | OscDouble Double |
--     OscChar Char | OscString String | OscSymbol String |
--     OscMidi {a :: Int32, b :: Int32, c :: Int32, d :: Int32}
--     deriving (Eq, Show, Typeable, Data)

-- oscTag :: OscValue -> Char
-- oscTag (OscNil) = 'N'
-- oscTag (OscBool x) = case x of
--     True -> 'T'
--     False -> 'F'
-- oscTag (OscTimeTag _ _) = 't'
-- oscTag (OscInt32 _) = 'i'
-- oscTag (OscInt64 _) = 'h'
-- oscTag (OscFloat _) = 'f'
-- oscTag (OscDouble _) = 'd'
-- oscTag (OscChar _) = 'c'
-- oscTag (OscString _) = 's'
-- oscTag (OscSymbol _) = 'S'
-- oscTag (OscMidi _ _ _ _) = 'm'


-- oscTagToConstr :: Map.Map Char Constr
-- oscTagToConstr = Map.fromList [('N', toConstr OscNil)]

-- data OscMessage = OscMessage {oscMsgPath :: String, oscMsgArgs :: [OscValue]}

-- data ClapiValue = Single OscValue | ClapiList [OscValue] deriving (Eq, Show)
-- type ClapiTags = Map.Map String ClapiValue

-- data ClapiMessage = ClapiMessage {
--     clapiMsgPath :: Path,
--     clapiMsgArgs :: [ClapiValue],
--     clapiMsgTags :: ClapiTags}

-- toOscValue :: ClapiValue -> [OscValue]
-- toOscValue (Single value) = [value]
-- -- FIMXE: inefficient concatenation I think
-- toOscValue (ClapiList values) = OscSymbol "[" : values ++ [OscSymbol "]"]

-- toOscArgs :: [ClapiValue] -> [OscValue]
-- toOscArgs = concatMap toOscValue

-- -- FIXME: these names are bad - things which convert to OSC _from_ tags, etc...
-- toOscTags :: ClapiTags -> [OscValue]
-- toOscTags = Map.foldrWithKey myFunc [] where
--     myFunc tagName clapiValue accumulator =
--         (OscSymbol "?") : (OscString tagName) :
--         toOscValue clapiValue ++ accumulator

-- toOscMessage :: ClapiMessage -> OscMessage
-- toOscMessage cm =
--   OscMessage pathString argsList where
--       pathString = toOsc $ clapiMsgPath cm
--       argsList = (toOscArgs $ clapiMsgArgs cm) ++ (toOscTags $ clapiMsgTags cm)

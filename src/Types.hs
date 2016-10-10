module Types
    (
        ClapiValue(..),
        ClapiPath,
        ClapiMethod(..),
        ClapiMessage(..),
        ClapiBundle,
        ClapiMessageTag,
    )
where

import Data.Word (Word32, Word64)
import Data.Int (Int32, Int64)

data ClapiMessage = CMessage {
    msgPath :: ClapiPath,
    msgMethod :: ClapiMethod,
    msgArgs :: [ClapiValue],
    msgTags :: [ClapiMessageTag]
} deriving (Eq, Show)

data ClapiValue = CNil | CBool Bool | CTime Word64 Word32 |
    CWord32 Word32 | CWord64 Word64 |
    CInt32 Int32 | CInt64 Int64 |
    CFloat Float | CDouble Double |
    CString String | CList [ClapiValue] deriving (Eq, Show)

type ClapiPath = [String]

data ClapiMethod = Error | Set | Add | Remove | Clear | Subscribe |
    Unsubscribe | AssignType | Children | Delete |
    Identify deriving (Eq, Show, Read, Enum, Bounded)

type ClapiMessageTag = (String, ClapiValue)

type ClapiBundle = [ClapiMessage]

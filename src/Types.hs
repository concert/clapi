module Types
    (
        Time(..),
        ClapiValue(..),
        fromClapiValue,
        toClapiValue,
        initLast,
        ClapiMethod(..),
        ClapiMessage(..),
        ClapiBundle,
        ClapiMessageTag,
    )
where

import Data.Word (Word32, Word64)
import Data.Int (Int32, Int64)
import qualified Data.Text as T

import Path (Path)

data ClapiMessage = CMessage {
    msgPath :: Path,
    msgMethod :: ClapiMethod,
    msgArgs :: [ClapiValue],
    msgTags :: [ClapiMessageTag]
} deriving (Eq, Show)

data Time = Time Word64 Word32 deriving (Eq, Show, Ord, Bounded)

data ClapiValue = CBool Bool | CTime Time |
    CWord32 Word32 | CWord64 Word64 |
    CInt32 Int32 | CInt64 Int64 |
    CFloat Float | CDouble Double |
    CString T.Text | CList [ClapiValue] deriving (Eq, Show)

class Clapiable a where
    toClapiValue :: a -> ClapiValue
    fromClapiValue :: ClapiValue -> a

instance Clapiable Bool where
    toClapiValue = CBool
    fromClapiValue (CBool x) = x

instance Clapiable Time where
    toClapiValue = CTime
    fromClapiValue (CTime x) = x

instance Clapiable Word32 where
    toClapiValue = CWord32
    fromClapiValue (CWord32 x) = x

instance Clapiable Word64 where
    toClapiValue = CWord64
    fromClapiValue (CWord64 x) = x

instance Clapiable Int32 where
    toClapiValue = CInt32
    fromClapiValue (CInt32 x) = x

instance Clapiable Int64 where
    toClapiValue = CInt64
    fromClapiValue (CInt64 x) = x

instance Clapiable Float where
    toClapiValue = CFloat
    fromClapiValue (CFloat x) = x

instance Clapiable Double where
    toClapiValue = CDouble
    fromClapiValue (CDouble x) = x

instance Clapiable T.Text where
    toClapiValue = CString
    fromClapiValue (CString x) = x

instance Clapiable a => Clapiable [a] where
    toClapiValue = CList . (map toClapiValue)
    fromClapiValue (CList xs) = map fromClapiValue xs

initLast :: [a] -> Maybe ([a], a)
initLast [] = Nothing
initLast as = Just $ internal as
  where
    internal (a':[]) = ([], a')
    internal (a:as) = accumulate (internal as) a
    accumulate (acc, a') a = (a:acc, a')

data ClapiMethod = Error | Set | Add | Remove | Clear | Subscribe |
    Unsubscribe | AssignType | Children | Delete
  deriving (Eq, Show, Read, Enum, Bounded)

type ClapiMessageTag = (String, ClapiValue)

type ClapiBundle = [ClapiMessage]

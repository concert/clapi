{-# LANGUAGE ExistentialQuantification #-}
module Types
    (
        CanFail,
        Time(..),
        ClapiValue(..),
        Enumerated(..),
        Clapiable,
        fromClapiValue,
        toClapiValue,
        ClapiMethod(..),
        ClapiMessage(..),
        ClapiBundle,
        ClapiMessageTag,
        InterpolationType(..),
        Interpolation(..),
        interpolation
    )
where

import Data.Word (Word8, Word32, Word64)
import Data.Int (Int32, Int64)
import qualified Data.Text as T

import Path (Path)

type CanFail a = Either String a

data ClapiMessage = CMessage {
    msgPath :: Path,
    msgMethod :: ClapiMethod,
    msgArgs :: [ClapiValue],
    msgTags :: [ClapiMessageTag]
} deriving (Eq, Show)

data Time = Time Word64 Word32 deriving (Eq, Show, Ord, Bounded)

data ClapiValue = CBool Bool | CTime Time |
    CEnum Word8 | CWord32 Word32 | CWord64 Word64 |
    CInt32 Int32 | CInt64 Int64 |
    CFloat Float | CDouble Double |
    CString T.Text | CList [ClapiValue] deriving (Eq, Ord)

instance Show ClapiValue where
    show x = '_' : (show' x)
      where
        show' (CBool x) = show x
        show' (CTime x) = show x
        show' (CEnum x) = show x
        show' (CWord32 x) = show x
        show' (CWord64 x) = show x
        show' (CInt32 x) = show x
        show' (CInt64 x) = show x
        show' (CFloat x) = show x
        show' (CDouble x) = show x
        show' (CString x) = show x
        show' (CList xs) = show xs

data Enumerated a = (Enum a, Bounded a) => Enumerated {getEnum :: a}
instance (Show a) => Show (Enumerated a) where
  show (Enumerated a) = "Enumerated " ++ show a

-- http://stackoverflow.com/questions/2743858/safe-and-polymorphic-toenum
safeToEnum :: (Enum a, Bounded a) => Int -> Maybe a
safeToEnum i =
  let
    r = toEnum i
    max = maxBound `asTypeOf` r
    min = minBound `asTypeOf` r
  in if fromEnum min <= i && i <= fromEnum max
  then Just r
  else Nothing

class Clapiable a where
    toClapiValue :: a -> ClapiValue
    fromClapiValue :: ClapiValue -> Maybe a

instance Clapiable Bool where
    toClapiValue = CBool
    fromClapiValue (CBool x) = Just x
    fromClapiValue _ = Nothing

instance Clapiable Time where
    toClapiValue = CTime
    fromClapiValue (CTime x) = Just x
    fromClapiValue _ = Nothing

instance (Enum a, Bounded a) => Clapiable (Enumerated a) where
    toClapiValue (Enumerated x) = CEnum $ fromIntegral $ fromEnum x
    fromClapiValue (CEnum x) = Enumerated <$> (safeToEnum $ fromIntegral x)
    fromClapiValue _ = Nothing

instance Clapiable Word32 where
    toClapiValue = CWord32
    fromClapiValue (CWord32 x) = Just x
    fromClapiValue _ = Nothing

instance Clapiable Word64 where
    toClapiValue = CWord64
    fromClapiValue (CWord64 x) = Just x
    fromClapiValue _ = Nothing

instance Clapiable Int32 where
    toClapiValue = CInt32
    fromClapiValue (CInt32 x) = Just x
    fromClapiValue _ = Nothing

instance Clapiable Int64 where
    toClapiValue = CInt64
    fromClapiValue (CInt64 x) = Just x
    fromClapiValue _ = Nothing

instance Clapiable Float where
    toClapiValue = CFloat
    fromClapiValue (CFloat x) = Just x
    fromClapiValue _ = Nothing

instance Clapiable Double where
    toClapiValue = CDouble
    fromClapiValue (CDouble x) = Just x
    fromClapiValue _ = Nothing

instance Clapiable T.Text where
    toClapiValue = CString
    fromClapiValue (CString x) = Just x
    fromClapiValue _ = Nothing

instance Clapiable a => Clapiable [a] where
    toClapiValue = CList . (fmap toClapiValue)
    fromClapiValue (CList xs) = sequence $ fmap fromClapiValue xs
    fromClapiValue _ = Nothing

data ClapiMethod = Error | Set | Add | Remove | Clear | Subscribe |
    Unsubscribe | AssignType | Children | Delete
  deriving (Eq, Show, Read, Enum, Bounded)

type ClapiMessageTag = (String, ClapiValue)

type ClapiBundle = [ClapiMessage]

data InterpolationType = ITConstant | ITLinear | ITBezier
  deriving (Show, Eq, Enum, Bounded)
data Interpolation = IConstant | ILinear | IBezier Word32 Word32
  deriving (Show, Eq)

interpolation :: InterpolationType -> [ClapiValue] -> CanFail Interpolation
interpolation ITConstant [] = Right $ IConstant
interpolation ITLinear [] = Right $ ILinear
interpolation ITBezier [CWord32 a, CWord32 b] = Right $ IBezier a b
interpolation _ _ = Left "Bad interpolation args"

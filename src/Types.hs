{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}
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
        Message(..),
        msgMethod',
        InterpolationType(..),
        Interpolation(..),
        interpolation
    )
where

import Prelude hiding (fail)
import Data.Either (either)
import Data.Word (Word8, Word32, Word64)
import Data.Int (Int32, Int64)
import qualified Data.Text as T
import Control.Monad.Fail (MonadFail, fail)

import Path (Path, Name)

type CanFail a = Either String a

instance MonadFail (Either String) where
    fail s = Left s

type Attributee = String
type Site = String

data Message =
    MsgError {msgPath' :: Path, msgErrTxt :: T.Text}
  | MsgSet {
        msgPath' :: Path,
        msgTime :: Time,
        msgArgs' :: [ClapiValue],
        msgInterpolation :: Interpolation,
        msgAttributee :: (Maybe Attributee),
        msgSite :: (Maybe Site)}
  | MsgAdd {
        msgPath' :: Path,
        msgTime :: Time,
        msgArgs' :: [ClapiValue],
        msgInterpolation :: Interpolation,
        msgAttributee :: (Maybe Attributee),
        msgSite :: (Maybe Site)}
  | MsgRemove {
        msgPath' :: Path,
        msgTime :: Time,
        msgAttributee :: (Maybe Attributee),
        msgSite :: (Maybe Site)}
  | MsgClear {
        msgPath' :: Path,
        msgTime :: Time,
        msgAttributee :: (Maybe Attributee),
        msgSite :: (Maybe Site)}
  | MsgSubscribe {msgPath' :: Path}
  | MsgUnsubscribe {msgPath' :: Path}
  | MsgAssignType {msgPath' :: Path, msgTypePath :: Path}
  | MsgDelete {msgPath' :: Path}
  | MsgChildren {msgPath' :: Path, msgChildren :: [Name]}
  deriving (Eq, Show)

msgMethod' :: Message -> ClapiMethod
msgMethod' (MsgError {}) = Error
msgMethod' (MsgSet {}) = Set
msgMethod' (MsgAdd {}) = Add
msgMethod' (MsgRemove {}) = Remove
msgMethod' (MsgClear {}) = Clear
msgMethod' (MsgSubscribe {}) = Subscribe
msgMethod' (MsgUnsubscribe {}) = Unsubscribe
msgMethod' (MsgAssignType {}) = AssignType
msgMethod' (MsgDelete {}) = Delete
msgMethod' (MsgChildren {}) = Children


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
safeToEnum :: (MonadFail m, Enum a, Bounded a) => Int -> m a
safeToEnum i =
  let
    r = toEnum i
    max = maxBound `asTypeOf` r
    min = minBound `asTypeOf` r
  in if fromEnum min <= i && i <= fromEnum max
  then return r
  else fail "enum value out of range"

class Clapiable a where
    toClapiValue :: a -> ClapiValue
    fromClapiValue :: (MonadFail m) => ClapiValue -> m a

instance Clapiable Bool where
    toClapiValue = CBool
    fromClapiValue (CBool x) = return x
    fromClapiValue _ = fail "bad type"

instance Clapiable Time where
    toClapiValue = CTime
    fromClapiValue (CTime x) = return x
    fromClapiValue _ = fail "bad type"

instance (Enum a, Bounded a) => Clapiable (Enumerated a) where
    toClapiValue (Enumerated x) = CEnum $ fromIntegral $ fromEnum x
    fromClapiValue (CEnum x) = Enumerated <$> (safeToEnum $ fromIntegral x)
    fromClapiValue _ = fail "bad type"

instance Clapiable Word32 where
    toClapiValue = CWord32
    fromClapiValue (CWord32 x) = return x
    fromClapiValue _ = fail "bad type"

instance Clapiable Word64 where
    toClapiValue = CWord64
    fromClapiValue (CWord64 x) = return x
    fromClapiValue _ = fail "bad type"

instance Clapiable Int32 where
    toClapiValue = CInt32
    fromClapiValue (CInt32 x) = return x
    fromClapiValue _ = fail "bad type"

instance Clapiable Int64 where
    toClapiValue = CInt64
    fromClapiValue (CInt64 x) = return x
    fromClapiValue _ = fail "bad type"

instance Clapiable Float where
    toClapiValue = CFloat
    fromClapiValue (CFloat x) = return x
    fromClapiValue _ = fail "bad type"

instance Clapiable Double where
    toClapiValue = CDouble
    fromClapiValue (CDouble x) = return x
    fromClapiValue _ = fail "bad type"

instance Clapiable T.Text where
    toClapiValue = CString
    fromClapiValue (CString x) = return x
    fromClapiValue _ = fail "bad type"

instance Clapiable a => Clapiable [a] where
    toClapiValue = CList . (fmap toClapiValue)
    fromClapiValue (CList xs) = sequence $ fmap fromClapiValue xs
    fromClapiValue _ = fail "bad type"

data ClapiMethod = Error | Set | Add | Remove | Clear | Subscribe |
    Unsubscribe | AssignType | Children | Delete
  deriving (Eq, Show, Read, Enum, Bounded)

data InterpolationType = ITConstant | ITLinear | ITBezier
  deriving (Show, Eq, Ord, Enum, Bounded)
data Interpolation = IConstant | ILinear | IBezier Word32 Word32
  deriving (Show, Eq)

interpolation :: InterpolationType -> [ClapiValue] -> CanFail Interpolation
interpolation ITConstant [] = Right $ IConstant
interpolation ITLinear [] = Right $ ILinear
interpolation ITBezier [CWord32 a, CWord32 b] = Right $ IBezier a b
interpolation _ _ = Left "Bad interpolation args"

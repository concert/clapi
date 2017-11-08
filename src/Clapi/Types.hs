{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}
module Clapi.Types
    (
        CanFail,
        ClapiTypeEnum(..),
        clapiValueType,
        Time(..),
        ClapiValue(..),
        Enumerated(..),
        Clapiable,
        fromClapiValue,
        toClapiValue,
        RequestBundle(..),
        UpdateBundle(..),
        Bundle(..),
        UMsgError(..),
        SubMessage(..),
        DataUpdateMessage(..),
        TreeUpdateMessage(..),
        OwnerUpdateMessage(..),
        ClapiMethod(..),
        Message(..),
        msgMethod',
        InterpolationType(..),
        Interpolation(..),
        interpolation,
        interpolationType
    )
where

import Prelude hiding (fail)
import Data.Either (either)
import Data.Word (Word8, Word32, Word64)
import Data.Int (Int32, Int64)
import qualified Data.Text as T
import Control.Monad.Fail (MonadFail, fail)

import Clapi.Path (Path, Name)

type CanFail a = Either String a

instance MonadFail (Either String) where
    fail s = Left s

type Attributee = String
type Site = String

class UMsg a where
   uMsgPath :: a -> Path

data UMsgError = UMsgError {errMsgPath :: Path, errMsgTxt :: T.Text} deriving (Eq, Show)

instance UMsg UMsgError where
    uMsgPath = errMsgPath

data SubMessage =
    UMsgSubscribe {subMsgPath :: Path}
  | UMsgUnsubscribe {subMsgPath :: Path}
  deriving (Eq, Show)

instance UMsg SubMessage where
    uMsgPath = subMsgPath

-- Separate because not valid in RequestBundle
data TreeUpdateMessage =
    UMsgAssignType {tuMsgPath :: Path, tuMsgTypePath :: Path}
  | UMsgDelete {tuMsgPath :: Path}
  deriving (Eq, Show)

instance UMsg TreeUpdateMessage where
    uMsgPath = tuMsgPath

data DataUpdateMessage =
    UMsgAdd {
        duMsgPath :: Path,
        duMsgTime :: Time,
        duMsgArgs :: [ClapiValue],
        duMsgInterpolation :: Interpolation,
        duMsgAttributee :: (Maybe Attributee),
        duMsgSite :: (Maybe Site)}
  | UMsgSet {
        duMsgPath :: Path,
        duMsgTime :: Time,
        duMsgArgs :: [ClapiValue],
        duMsgInterpolation :: Interpolation,
        duMsgAttributee :: (Maybe Attributee),
        duMsgSite :: (Maybe Site)}
  | UMsgRemove {
        duMsgPath :: Path,
        duMsgTime :: Time,
        duMsgAttributee :: (Maybe Attributee),
        duMsgSite :: (Maybe Site)}
  | UMsgClear {
        duMsgPath :: Path,
        duMsgTime :: Time,
        duMsgAttributee :: (Maybe Attributee),
        duMsgSite :: (Maybe Site)}
  | UMsgSetChildren {
        duMsgPath :: Path,
        duMsgNames :: [Name],
        duMsgAttributee :: (Maybe Attributee)}
   deriving (Eq, Show)

instance UMsg DataUpdateMessage where
    uMsgPath = duMsgPath

type OwnerUpdateMessage = Either TreeUpdateMessage DataUpdateMessage

data UpdateBundle = UpdateBundle {ubErrs :: [UMsgError], ubMsgs :: [OwnerUpdateMessage]} deriving (Eq, Show)
data RequestBundle = RequestBundle {rbSubs :: [SubMessage], rbMsgs :: [DataUpdateMessage]} deriving (Eq, Show)

type Bundle = Either UpdateBundle RequestBundle


-- Existing message type

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


-- Values:

data Time = Time Word64 Word32 deriving (Eq, Show, Ord, Bounded)

data ClapiTypeEnum
  = ClTTime | ClTEnum | ClTWord32 | ClTWord64 | ClTInt32 | ClTInt64
  | ClTFloat | ClTDouble | ClTString | ClTList
  deriving (Eq, Ord, Show, Enum, Bounded)

data ClapiValue = ClTime Time |
    ClEnum Word8 | ClWord32 Word32 | ClWord64 Word64 |
    ClInt32 Int32 | ClInt64 Int64 |
    ClFloat Float | ClDouble Double |
    ClString T.Text | ClList [ClapiValue] deriving (Eq, Ord)

instance Show ClapiValue where
    show x = '_' : (show' x)
      where
        show' (ClTime x) = show x
        show' (ClEnum x) = show x
        show' (ClWord32 x) = show x
        show' (ClWord64 x) = show x
        show' (ClInt32 x) = show x
        show' (ClInt64 x) = show x
        show' (ClFloat x) = show x
        show' (ClDouble x) = show x
        show' (ClString x) = show x
        show' (ClList xs) = show xs

clapiValueType :: ClapiValue -> ClapiTypeEnum
clapiValueType (ClTime _) = ClTTime
clapiValueType (ClEnum _) = ClTEnum
clapiValueType (ClWord32 _) = ClTWord32
clapiValueType (ClWord64 _) = ClTWord64
clapiValueType (ClInt32 _) = ClTInt32
clapiValueType (ClInt64 _) = ClTInt64
clapiValueType (ClFloat _) = ClTFloat
clapiValueType (ClDouble _) = ClTDouble
clapiValueType (ClString _) = ClTString
clapiValueType (ClList _) = ClTList

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

instance Clapiable Time where
    toClapiValue = ClTime
    fromClapiValue (ClTime x) = return x
    fromClapiValue _ = fail "bad type"

instance (Enum a, Bounded a) => Clapiable (Enumerated a) where
    toClapiValue (Enumerated x) = ClEnum $ fromIntegral $ fromEnum x
    fromClapiValue (ClEnum x) = Enumerated <$> (safeToEnum $ fromIntegral x)
    fromClapiValue _ = fail "bad type"

instance Clapiable Word32 where
    toClapiValue = ClWord32
    fromClapiValue (ClWord32 x) = return x
    fromClapiValue _ = fail "bad type"

instance Clapiable Word64 where
    toClapiValue = ClWord64
    fromClapiValue (ClWord64 x) = return x
    fromClapiValue _ = fail "bad type"

instance Clapiable Int32 where
    toClapiValue = ClInt32
    fromClapiValue (ClInt32 x) = return x
    fromClapiValue _ = fail "bad type"

instance Clapiable Int64 where
    toClapiValue = ClInt64
    fromClapiValue (ClInt64 x) = return x
    fromClapiValue _ = fail "bad type"

instance Clapiable Float where
    toClapiValue = ClFloat
    fromClapiValue (ClFloat x) = return x
    fromClapiValue _ = fail "bad type"

instance Clapiable Double where
    toClapiValue = ClDouble
    fromClapiValue (ClDouble x) = return x
    fromClapiValue _ = fail "bad type"

instance Clapiable T.Text where
    toClapiValue = ClString
    fromClapiValue (ClString x) = return x
    fromClapiValue _ = fail "bad type"

instance Clapiable a => Clapiable [a] where
    toClapiValue = ClList . (fmap toClapiValue)
    fromClapiValue (ClList xs) = sequence $ fmap fromClapiValue xs
    fromClapiValue _ = fail "bad type"

data ClapiMethod = Error | Set | Add | Remove | Clear | Subscribe |
    Unsubscribe | AssignType | Children | Delete
  deriving (Eq, Show, Read, Enum, Bounded)

data InterpolationType = ITConstant | ITLinear | ITBezier
  deriving (Show, Eq, Ord, Enum, Bounded)
data Interpolation = IConstant | ILinear | IBezier Word32 Word32
  deriving (Show, Eq, Ord)

interpolation :: InterpolationType -> [ClapiValue] -> CanFail Interpolation
interpolation ITConstant [] = Right $ IConstant
interpolation ITLinear [] = Right $ ILinear
interpolation ITBezier [ClWord32 a, ClWord32 b] = Right $ IBezier a b
interpolation _ _ = Left "Bad interpolation args"

interpolationType :: Interpolation -> InterpolationType
interpolationType IConstant = ITConstant
interpolationType ILinear = ITLinear
interpolationType (IBezier _ _) = ITBezier

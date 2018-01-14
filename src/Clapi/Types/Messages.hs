{-# OPTIONS_GHC -Wall -Wno-orphans #-}

module Clapi.Types.Messages where

import qualified Data.Text as T

import Clapi.Types.Base (Attributee, Site, Time, Interpolation)
import Clapi.Types.Path (Path, Seg)
import Clapi.Types.Wire (WireValue)

class Msg a where
   uMsgPath :: a -> Path

data MsgError
  = MsgError {errMsgPath :: Path, errMsgTxt :: T.Text} deriving (Eq, Show)

instance Msg MsgError where
    uMsgPath = errMsgPath

data SubMessage =
    MsgSubscribe {subMsgPath :: Path}
  | MsgUnsubscribe {subMsgPath :: Path}
  deriving (Eq, Show)

instance Msg SubMessage where
    uMsgPath = subMsgPath

-- Separate because not valid in RequestBundle
data TreeUpdateMessage =
    MsgAssignType {tuMsgPath :: Path, tuMsgTypePath :: Path}
  | MsgDelete {tuMsgPath :: Path}
  deriving (Eq, Show)

instance Msg TreeUpdateMessage where
    uMsgPath = tuMsgPath

data DataUpdateMessage =
    MsgAdd
      { duMsgPath :: Path
      , duMsgTime :: Time
      , duMsgArgs :: [WireValue]
      , duMsgInterpolation :: Interpolation
      , duMsgAttributee :: (Maybe Attributee)
      , duMsgSite :: (Maybe Site)
      }
  | MsgSet
      { duMsgPath :: Path
      , duMsgTime :: Time
      , duMsgArgs :: [WireValue]
      , duMsgInterpolation :: Interpolation
      , duMsgAttributee :: (Maybe Attributee)
      , duMsgSite :: (Maybe Site)
      }
  | MsgRemove
      { duMsgPath :: Path
      , duMsgTime :: Time
      , duMsgAttributee :: (Maybe Attributee)
      , duMsgSite :: (Maybe Site)
      }
  | MsgClear
      { duMsgPath :: Path
      , duMsgTime :: Time
      , duMsgAttributee :: (Maybe Attributee)
      , duMsgSite :: (Maybe Site)
      }
  | MsgSetChildren
      { duMsgPath :: Path
      , duMsgNames :: [Seg]
      , duMsgAttributee :: (Maybe Attributee)
      }
   deriving (Eq, Show)

instance Msg DataUpdateMessage where
    uMsgPath = duMsgPath

type OwnerUpdateMessage = Either TreeUpdateMessage DataUpdateMessage

instance (Msg a, Msg b) => Msg (Either a b) where
    uMsgPath = either uMsgPath uMsgPath

data UpdateBundle = UpdateBundle {ubErrs :: [MsgError], ubMsgs :: [OwnerUpdateMessage]} deriving (Eq, Show)
data RequestBundle = RequestBundle {rbSubs :: [SubMessage], rbMsgs :: [DataUpdateMessage]} deriving (Eq, Show)
data OwnerRequestBundle = OwnerRequestBundle {orbErrs :: [MsgError], orbMsgs :: [DataUpdateMessage]} deriving (Eq, Show)

data ToRelayBundle = TRBClient RequestBundle | TRBOwner UpdateBundle deriving (Eq, Show)
data FromRelayBundle = FRBClient UpdateBundle | FRBOwner OwnerRequestBundle deriving (Eq, Show)

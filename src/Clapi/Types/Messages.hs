{-# OPTIONS_GHC -Wall -Wno-orphans #-}

module Clapi.Types.Messages where

import qualified Data.Text as Text

import Clapi.Types.Base (Attributee, Site, Time, Interpolation)
import Clapi.Types.Path (Seg, NodePath, TypePath, isParentOf)
import Clapi.Types.Wire (WireValue)

class Msg a where
   uMsgPath :: a -> NodePath

data MsgError
  = MsgError {errMsgPath :: NodePath, errMsgTxt :: Text} deriving (Eq, Show)

instance Msg MsgError where
    uMsgPath = errMsgPath

data SubMessage =
    MsgSubscribe {subMsgPath :: NodePath}
  | MsgUnsubscribe {subMsgPath :: NodePath}
  deriving (Eq, Show)

instance Msg SubMessage where
    uMsgPath = subMsgPath

-- Separate because not valid in RequestBundle
data TreeUpdateMessage =
    MsgAssignType {tuMsgPath :: NodePath, tuMsgTypePath :: TypePath}
  | MsgDelete {tuMsgPath :: NodePath}
  deriving (Eq, Show)

instance Msg TreeUpdateMessage where
    uMsgPath = tuMsgPath

data DataUpdateMessage
  = MsgConstSet
      { duMsgPath :: NodePath
      , duMsgArgs :: [WireValue]
      , duMsgAttributee :: Maybe Attributee
      , duMsgSite :: Maybe Site
      }
  | MsgConstClear
      { duMsgPath :: NodePath
      , duMsgAttributee :: Maybe Attributee
      , duMsgSite :: Maybe Site
      }
  | MsgSet
      { duMsgPath :: NodePath
      , duMsgTsUuid :: Word32
      , duMsgTime :: Time
      , duMsgArgs :: [WireValue]
      , duMsgInterpolation :: Interpolation
      , duMsgAttributee :: Maybe Attributee
      , duMsgSite :: Maybe Site
      }
  | MsgRemove
      { duMsgPath :: NodePath
      , duMsgTsUuid :: Word32
      , duMsgAttributee :: Maybe Attributee
      , duMsgSite :: Maybe Site
      }
  | MsgClear
      { duMsgPath :: NodePath
      , duMsgTsUuid :: Word32
      , duMsgAttributee :: Maybe Attributee
      , duMsgSite :: Maybe Site
      }
  | MsgSetChildren
      { duMsgPath :: NodePath
      , duMsgNames :: UniqList Seg
      , duMsgAttributee :: Maybe Attributee
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

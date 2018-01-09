{-# OPTIONS_GHC -Wall -Wno-orphans #-}

module Clapi.Types.Messages where

import qualified Data.Text as T

import Clapi.Path (Path, Seg)
import Clapi.Types.Base (Attributee, Site, Time, Interpolation)
import Clapi.Types.Wire (WireValue)

class UMsg a where
   uMsgPath :: a -> Path

data UMsgError
  = UMsgError {errMsgPath :: Path, errMsgTxt :: T.Text} deriving (Eq, Show)

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
    UMsgAdd
      { duMsgPath :: Path
      , duMsgTime :: Time
      , duMsgArgs :: [WireValue]
      , duMsgInterpolation :: Interpolation
      , duMsgAttributee :: (Maybe Attributee)
      , duMsgSite :: (Maybe Site)
      }
  | UMsgSet
      { duMsgPath :: Path
      , duMsgTime :: Time
      , duMsgArgs :: [WireValue]
      , duMsgInterpolation :: Interpolation
      , duMsgAttributee :: (Maybe Attributee)
      , duMsgSite :: (Maybe Site)
      }
  | UMsgRemove
      { duMsgPath :: Path
      , duMsgTime :: Time
      , duMsgAttributee :: (Maybe Attributee)
      , duMsgSite :: (Maybe Site)
      }
  | UMsgClear
      { duMsgPath :: Path
      , duMsgTime :: Time
      , duMsgAttributee :: (Maybe Attributee)
      , duMsgSite :: (Maybe Site)
      }
  | UMsgSetChildren
      { duMsgPath :: Path
      , duMsgNames :: [Seg]
      , duMsgAttributee :: (Maybe Attributee)
      }
   deriving (Eq, Show)

instance UMsg DataUpdateMessage where
    uMsgPath = duMsgPath

type OwnerUpdateMessage = Either TreeUpdateMessage DataUpdateMessage

instance (UMsg a, UMsg b) => UMsg (Either a b) where
    uMsgPath = either uMsgPath uMsgPath

data UpdateBundle = UpdateBundle {ubErrs :: [UMsgError], ubMsgs :: [OwnerUpdateMessage]} deriving (Eq, Show)
data RequestBundle = RequestBundle {rbSubs :: [SubMessage], rbMsgs :: [DataUpdateMessage]} deriving (Eq, Show)
data OwnerRequestBundle = OwnerRequestBundle {orbErrs :: [UMsgError], orbMsgs :: [DataUpdateMessage]} deriving (Eq, Show)

data ToRelayBundle = TRBClient RequestBundle | TRBOwner UpdateBundle deriving (Eq, Show)
data FromRelayBundle = FRBClient UpdateBundle | FRBOwner OwnerRequestBundle deriving (Eq, Show)

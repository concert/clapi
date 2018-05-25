{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE
   PatternSynonyms
 , DataKinds
 , DeriveFunctor
#-}

module Clapi.Types.Messages where

import Data.Text (Text)
import Data.Word (Word32)

import Clapi.Types.Base (Attributee, Time, Interpolation)
import Clapi.Types.Definitions
  ( TreeDefinition, TupleDefinition, Mandatoriness, ClientPermission
  , StructDefinition)
import Clapi.Types.Path (Seg, Path, pattern (:</))
import Clapi.Types.TypeName
  ( TypeName, ChildTypeName(..), TypeNamespace(..), rawChildTypeName, ChildSeg
  , promoteChildSeg, AnyTypeName)
import qualified Clapi.Types.Path as Path
import Clapi.Types.Wire (WireValue)

-- FIXME: redefinition
type TpId = Word32

data ErrorIndex a
  = GlobalError
  | PathError Path
  | TimePointError Path TpId
  | TypeError a
  deriving (Show, Eq, Ord, Functor)

splitErrIdx :: ErrorIndex (ChildTypeName a) -> Maybe (Seg, ErrorIndex Seg)
splitErrIdx ei = case ei of
  GlobalError -> Nothing
  PathError p -> fmap PathError <$> Path.splitHead p
  TimePointError p tpid -> fmap (flip TimePointError tpid) <$> Path.splitHead p
  TypeError ctn -> Just $ TypeError <$> rawChildTypeName ctn

namespaceErrIdx :: Seg -> ErrorIndex (ChildSeg a) -> ErrorIndex (ChildTypeName a)
namespaceErrIdx ns ei = case ei of
  GlobalError -> GlobalError
  PathError p -> PathError $ ns :</ p
  TimePointError p tpid -> TimePointError (ns :</ p) tpid
  TypeError s -> TypeError $ promoteChildSeg ns s

data MsgError a
  = MsgError {errIndex :: ErrorIndex a, errMsgTxt :: Text} deriving (Eq, Show)

data DefMessage a d
  = MsgDefine a d
  | MsgUndefine a
  deriving (Show, Eq)

data SubMessage
  = MsgSubscribe {subMsgPath :: Path}
  | MsgTypeSubscribe {subMsgTypeName :: AnyTypeName}
  | MsgUnsubscribe {subMsgPath :: Path}
  | MsgTypeUnsubscribe {subMsgTypeName :: AnyTypeName}
  deriving (Eq, Show)

data TypeMessage = MsgAssignType Path (ChildTypeName 'TnTree) ClientPermission deriving (Show, Eq)

data DataUpdateMessage
  = MsgConstSet
      { duMsgPath :: Path
      , duMsgArgs :: [WireValue]
      , duMsgAttributee :: Maybe Attributee
      }
  | MsgSet
      { duMsgPath :: Path
      , duMsgTpId :: TpId
      , duMsgTime :: Time
      , duMsgArgs :: [WireValue]
      , duMsgInterpolation :: Interpolation
      , duMsgAttributee :: Maybe Attributee
      }
  | MsgRemove
      { duMsgPath :: Path
      , duMsgTpId :: Word32
      , duMsgAttributee :: Maybe Attributee
      }
   deriving (Eq, Show)

data ContainerUpdateMessage
  = MsgPresentAfter
      { cuMsgPath :: Path
      , cuMsgTarg :: Seg
      , cuMsgRef :: Maybe Seg
      , cuMsgAttributee :: Maybe Attributee
      }
  | MsgAbsent
      { cuMsgPath :: Path
      , cuMsgTarg :: Seg
      , cuMsgAttributee :: Maybe Attributee
      }
  deriving (Eq, Show)

data ToRelayProviderBundle = ToRelayProviderBundle
  { trpbNamespace :: Seg
  , trpbErrors :: [MsgError Seg]
  , trpbTreeDefs :: [DefMessage Seg TreeDefinition]
  , trpbCreateDefs :: [DefMessage Seg (StructDefinition Mandatoriness)]
  , trpbValueDefs :: [DefMessage Seg TupleDefinition]
  , trpbData :: [DataUpdateMessage]
  , trpbContMsgs :: [ContainerUpdateMessage]
  } deriving (Show, Eq)

data ToRelayProviderRelinquish
  = ToRelayProviderRelinquish Seg deriving (Show, Eq)

data FromRelayProviderBundle = FromRelayProviderBundle
  { frpbNamespace :: Seg
  , frpbData :: [DataUpdateMessage]
  , frpbContMsgs :: [ContainerUpdateMessage]
  } deriving (Show, Eq)

data FromRelayProviderErrorBundle = FromRelayProviderErrorBundle
  { frpebErrors :: [MsgError AnyTypeName]
  } deriving (Eq, Show)

data ToRelayClientBundle = ToRelayClientBundle
  { trcbSubs :: [SubMessage]
  , trcbData :: [DataUpdateMessage]
  , trcbContMsgs :: [ContainerUpdateMessage]
  } deriving (Eq, Show)

data FromRelayClientBundle = FromRelayClientBundle
  { frcbTreeTypeUnsubs :: [TypeName 'TnTree]
  , frcbCreateTypeUnsubs :: [TypeName 'TnCreate]
  , frcbValueTypeUnsubs :: [TypeName 'TnValue]
  , frcbDataUnsubs :: [Path]
  , frcbErrors :: [MsgError AnyTypeName]
  , frcbTreeDefinitions :: [DefMessage (TypeName 'TnTree) TreeDefinition]
  , frcbCreateDefinitions :: [DefMessage (TypeName 'TnCreate) (StructDefinition Mandatoriness)]
  , frcbValueDefinitions :: [DefMessage (TypeName 'TnValue) TupleDefinition]
  , frcbTypeAssignments :: [TypeMessage]
  , frcbData :: [DataUpdateMessage]
  , frcbContMsgs :: [ContainerUpdateMessage]
  } deriving (Show, Eq)

data ToRelayBundle
  = Trpb ToRelayProviderBundle
  | Trpr ToRelayProviderRelinquish
  | Trcb ToRelayClientBundle
  deriving (Show, Eq)

data FromRelayBundle
  = Frpb FromRelayProviderBundle
  | Frpeb FromRelayProviderErrorBundle
  | Frcb FromRelayClientBundle
  deriving (Show, Eq)

module Clapi.Types.Messages where

import Data.Bifunctor (bimap)
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import Data.Word (Word32)

import Clapi.Types.Base (Attributee, Time, Interpolation)
import Clapi.Types.Definitions (Definition, Liberty, PostDefinition)
import Clapi.Types.Path
  (Seg, Path, TypeName(..), qualify, unqualify, pattern (:</), Namespace(..))
import qualified Clapi.Types.Path as Path
import Clapi.Types.Wire (WireValue)

-- FIXME: redefinition
type TpId = Word32

data ErrorIndex a
  = GlobalError
  | PathError Path
  | TimePointError Path TpId
  | PostTypeError (Tagged PostDefinition a)
  | TypeError (Tagged Definition a)
  deriving (Show, Eq, Ord)

splitErrIdx :: ErrorIndex TypeName -> Maybe (Namespace, ErrorIndex Seg)
splitErrIdx ei = case ei of
  GlobalError -> Nothing
  PathError p -> bimap Namespace PathError <$> Path.splitHead p
  TimePointError p tpid -> bimap Namespace (flip TimePointError tpid) <$>
    Path.splitHead p
  PostTypeError tn -> Just $ fmap PostTypeError $ unqualify tn
  TypeError tn -> Just $ fmap TypeError $ unqualify tn

namespaceErrIdx :: Namespace -> ErrorIndex Seg -> ErrorIndex TypeName
namespaceErrIdx ns ei = case ei of
  GlobalError -> GlobalError
  PathError p -> PathError $ unNamespace ns :</ p
  TimePointError p tpid -> TimePointError (unNamespace ns :</ p) tpid
  PostTypeError s -> PostTypeError $ qualify ns s
  TypeError s -> TypeError $ qualify ns s

data MsgError a
  = MsgError {errIndex :: ErrorIndex a, errMsgTxt :: Text} deriving (Eq, Show)

data DefMessage ident def
  = MsgDefine ident def
  | MsgUndefine ident
  deriving (Show, Eq)

-- FIXME: might be nicer to break this up into sub and unsub values typed by
-- what they are subscriptions for:
data SubMessage
  = MsgSubscribe {subMsgPath :: Path}
  | MsgPostTypeSubscribe {subMsgPostTypeName :: Tagged PostDefinition TypeName}
  | MsgTypeSubscribe {subMsgTypeName :: Tagged Definition TypeName}
  | MsgUnsubscribe {subMsgPath :: Path}
  | MsgPostTypeUnsubscribe
    {subMsgPostTypeName :: Tagged PostDefinition TypeName}
  | MsgTypeUnsubscribe {subMsgTypeName :: Tagged Definition TypeName}
  deriving (Eq, Show)

data TypeMessage
  = MsgAssignType Path (Tagged Definition TypeName) Liberty
  deriving (Show, Eq)

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
  = MsgCreateAfter
      { cuMsgPath :: Path
      , cuMsgArgs :: [WireValue]
      , cuMsgPlaceholder :: Seg
      , cuMsgRef :: Maybe Seg
      , cuMsgAttributee :: Maybe Attributee
      }
  | MsgMoveAfter
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
  { trpbNamespace :: Namespace
  , trpbErrors :: [MsgError Seg]
  , trpbPostDefs :: [DefMessage (Tagged PostDefinition Seg) PostDefinition]
  , trpbDefinitions :: [DefMessage (Tagged Definition Seg) Definition]
  , trpbData :: [DataUpdateMessage]
  , trpbContMsgs :: [ContainerUpdateMessage]
  } deriving (Show, Eq)

data ToRelayProviderRelinquish
  = ToRelayProviderRelinquish Namespace deriving (Show, Eq)

data FromRelayProviderBundle = FromRelayProviderBundle
  { frpbNamespace :: Namespace
  , frpbData :: [DataUpdateMessage]
  , frpbContMsgs :: [ContainerUpdateMessage]
  } deriving (Show, Eq)

data FromRelayProviderErrorBundle = FromRelayProviderErrorBundle
  { frpebErrors :: [MsgError TypeName]
  } deriving (Eq, Show)

data ToRelayClientBundle = ToRelayClientBundle
  { trcbSubs :: [SubMessage]
  , trcbData :: [DataUpdateMessage]
  , trcbContMsgs :: [ContainerUpdateMessage]
  } deriving (Eq, Show)

data FromRelayClientBundle = FromRelayClientBundle
  { frcbPostTypeUnsubs :: [Tagged PostDefinition TypeName]
  , frcbTypeUnsubs :: [Tagged Definition TypeName]
  , frcbDataUnsubs :: [Path]
  , frcbErrors :: [MsgError TypeName]
  , frcbPostDefs :: [DefMessage (Tagged PostDefinition TypeName) PostDefinition]
  , frcbDefinitions :: [DefMessage (Tagged Definition TypeName) Definition]
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

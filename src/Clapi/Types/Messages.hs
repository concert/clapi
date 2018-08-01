module Clapi.Types.Messages where

import Data.Tagged (Tagged(..))
import Data.Text (Text)
import Data.Word (Word32)

import Clapi.Types.Base (Attributee, Time, Interpolation)
import Clapi.Types.Definitions (Definition, Liberty, PostDefinition)
import Clapi.Types.Path
  (Seg, Path, TypeName(..), Namespace(..), Placeholder(..))
import Clapi.Types.Wire (WireValue)

-- FIXME: redefinition
type TpId = Word32

data DataErrorIndex
  = GlobalError
  | PathError Path
  | TimePointError Path TpId
  -- Placeholder errors need to go somewhere, but potentially not in the data
  -- errs? Perhaps we should remove global errors too and make a bigger sum type
  -- on top?!
  -- | PlaceholderError Placeholder
  deriving (Show, Eq, Ord)

data DataErrorMessage
  = MsgDataError {dataErrIndex :: DataErrorIndex, dataErrTxt :: Text}
  deriving (Eq, Show)

data DefMessage ident def
  = MsgDefine ident def
  | MsgUndefine ident
  deriving (Show, Eq)

data SubErrorIndex
  = PostTypeSubError (Tagged PostDefinition TypeName)
  | TypeSubError (Tagged Definition TypeName)
  | PathSubError Path
  deriving (Show, Eq, Ord)

data SubErrorMessage
  = MsgSubError {subErrIndex :: SubErrorIndex, subErrTxt :: Text}
  deriving (Eq, Show)

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

data ToProviderContainerUpdateMessage
  = TpcumCreateAfter
      -- FIXME: nested wire values is a legacy hangover because we still have
      -- [WireValue] in tree data nodes representing single "values":
      { tpcumArgs :: [[WireValue]]
      , tpcumPlaceholder :: Placeholder
      , tpcumRef :: Maybe (Either Placeholder Seg)
      , tpcumAtt :: Maybe Attributee
      }
  | TpcumMoveAfter
      { tpcumTarg :: Seg
      , tpcumRef :: Maybe (Either Placeholder Seg)
      , tpcumAtt :: Maybe Attributee
      }
  | TpcumAbsent
      { tpcumTarg :: Seg
      , tpcumAtt :: Maybe Attributee
      }
  deriving (Eq, Show)

data ToClientContainerUpdateMessage
  = TccumPresentAfter
      { pcuMsgTarg :: Seg
      , pcuMsgRef :: Maybe Seg
      , pcuMsgAtt :: Maybe Attributee
      }
  | TccumAbsent
      { pcuMsgTarg :: Seg
      , pcuMsgAtt :: Maybe Attributee
      }
  deriving (Eq, Show)

data ToRelayProviderBundle = ToRelayProviderBundle
  { trpbNamespace :: Namespace
  , trpbErrors :: [DataErrorMessage]
  , trpbPostDefs :: [DefMessage (Tagged PostDefinition Seg) PostDefinition]
  , trpbDefinitions :: [DefMessage (Tagged Definition Seg) Definition]
  , trpbData :: [DataUpdateMessage]
  , trpbContMsgs :: [(Path, ToClientContainerUpdateMessage)]
  } deriving (Show, Eq)

newtype ToRelayProviderRelinquish
  = ToRelayProviderRelinquish Namespace deriving (Show, Eq)

data FromRelayProviderBundle = FromRelayProviderBundle
  { frpbNamespace :: Namespace
  , frpbData :: [DataUpdateMessage]
  , frpbContMsgs :: [(Path, ToProviderContainerUpdateMessage)]
  } deriving (Show, Eq)

newtype FromRelayProviderErrorBundle = FromRelayProviderErrorBundle
  { frpebErrors :: [DataErrorMessage]
  } deriving (Eq, Show)

newtype ToRelayClientSubBundle = ToRelayClientSubBundle
  -- FIXME: want to break this down so that we can no longer subscribe to root
  -- (i.e. all subs are (Namespace, Path or Seg))
  { trcsbSubs :: [SubMessage]
  } deriving (Eq, Show)

data ToRelayClientUpdateBundle = ToRelayClientUpdateBundle
  { trcbNamespace :: Namespace
  , trcbData :: [DataUpdateMessage]
  , trcbContMsgs :: [(Path, ToProviderContainerUpdateMessage)]
  } deriving (Eq, Show)

newtype FromRelayClientRootBundle = FromRelayClientRootBundle
  { frcrbContMsgs :: [ToClientContainerUpdateMessage]
  } deriving (Eq, Show)

data FromRelayClientSubBundle = FromRelayClientSubBundle
  { frcsbSubErrs :: [SubErrorMessage]
  , frcsbPostTypeUnsubs :: [Tagged PostDefinition TypeName]
  , frcsbTypeUnsubs :: [Tagged Definition TypeName]
  , frcsbDataUnsubs :: [Path]
  } deriving (Eq, Show)

data FromRelayClientUpdateBundle = FromRelayClientUpdateBundle
  { frcbNamespace :: Namespace
  , frcbErrors :: [DataErrorMessage]
  , frcbPostDefs :: [DefMessage (Tagged PostDefinition Seg) PostDefinition]
  , frcbDefinitions :: [DefMessage (Tagged Definition Seg) Definition]
  , frcbTypeAssignments :: [TypeMessage]
  , frcbData :: [DataUpdateMessage]
  , frcbContMsgs :: [(Path, ToClientContainerUpdateMessage)]
  } deriving (Show, Eq)

data ToRelayBundle
  = Trpb ToRelayProviderBundle
  | Trpr ToRelayProviderRelinquish
  | Trcsb ToRelayClientSubBundle
  | Trcub ToRelayClientUpdateBundle
  deriving (Show, Eq)

data FromRelayBundle
  = Frpb FromRelayProviderBundle
  | Frpeb FromRelayProviderErrorBundle
  | Frcrb FromRelayClientRootBundle
  | Frcsb FromRelayClientSubBundle
  | Frcub FromRelayClientUpdateBundle
  deriving (Show, Eq)

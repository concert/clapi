{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE PatternSynonyms #-}

module Clapi.Types.Messages where

import Data.Maybe (fromJust)
import Data.Either (partitionEithers)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Monoid
import Data.Text (Text)
import Data.Word (Word32)

import Clapi.Types.Base (Attributee, Time, Interpolation)
import Clapi.Types.Definitions (Definition, Liberty)
import Clapi.Types.Path (Seg, Path, TypeName(..), pattern (:</))
import qualified Clapi.Types.Path as Path
import Clapi.Types.Wire (WireValue)
import Clapi.Types.UniqList (UniqList)

-- FIXME: redefinition
type TpId = Word32

data ErrorIndex a
  = GlobalError
  | PathError Path
  | TimePointError Path TpId
  | TypeNameError a
  deriving (Show, Eq, Ord)

splitErrIdx :: ErrorIndex TypeName -> Maybe (Seg, ErrorIndex Seg)
splitErrIdx ei = case ei of
  GlobalError -> Nothing
  PathError p -> fmap PathError <$> Path.splitHead p
  TimePointError p tpid -> fmap (flip TimePointError tpid) <$> Path.splitHead p
  TypeNameError (TypeName ns s) -> Just (ns, TypeNameError s)

namespaceErrIdx :: Seg -> ErrorIndex Seg -> ErrorIndex TypeName
namespaceErrIdx ns ei = case ei of
  GlobalError -> GlobalError
  PathError p -> PathError $ ns :</ p
  TimePointError p tpid -> TimePointError (ns :</ p) tpid
  TypeNameError s -> TypeNameError $ TypeName ns s

data MsgError a
  = MsgError {errIndex :: ErrorIndex a, errMsgTxt :: Text} deriving (Eq, Show)

data DefMessage a
  = MsgDefine a Definition
  | MsgUndefine a
  deriving (Show, Eq)

data SubMessage
  = MsgSubscribe {subMsgPath :: Path}
  | MsgTypeSubscribe {subMsgTypeName :: TypeName}
  | MsgUnsubscribe {subMsgPath :: Path}
  | MsgTypeUnsubscribe {subMsgTypeName :: TypeName}
  deriving (Eq, Show)

data TypeMessage = MsgAssignType Path TypeName Liberty deriving (Show, Eq)

data DataUpdateMessage
  = MsgConstSet
      { duMsgPath :: Path
      , duMsgArgs :: [WireValue]
      , duMsgAttributee :: Maybe Attributee
      }
  | MsgSet
      { duMsgPath :: Path
      , duMsgTsUuid :: TpId
      , duMsgTime :: Time
      , duMsgArgs :: [WireValue]
      , duMsgInterpolation :: Interpolation
      , duMsgAttributee :: Maybe Attributee
      }
  | MsgRemove
      { duMsgPath :: Path
      , duMsgTsUuid :: Word32
      , duMsgAttributee :: Maybe Attributee
      }
  | MsgSetChildren
      { duMsgPath :: Path
      , duMsgNames :: UniqList Seg
      , duMsgAttributee :: Maybe Attributee
      }
   deriving (Eq, Show)

data ToRelayProviderBundle = ToRelayProviderBundle
  { trpbNamespace :: Seg
  , trpbErrors :: [MsgError Seg]
  , trpbDefinitions :: [DefMessage Seg]
  , trpbData :: [DataUpdateMessage]
  } deriving (Show, Eq)

data ToRelayProviderRelinquish
  = ToRelayProviderRelinquish Seg deriving (Show, Eq)

data FromRelayProviderBundle = FromRelayProviderBundle
  { frpbNamespace :: Seg
  , frpbData :: [DataUpdateMessage]
  } deriving (Show, Eq)

data FromRelayProviderErrorBundle = FromRelayProviderErrorBundle
  { frpebNamespace :: Seg
  , frpebErrors :: [MsgError Seg]
  } deriving (Eq, Show)

data ToRelayClientBundle = ToRelayClientBundle
  { trcbSubs :: [SubMessage]
  , trcbData :: [DataUpdateMessage]
  } deriving (Eq, Show)

data FromRelayClientBundle = FromRelayClientBundle
  { frcbErrors :: [MsgError Path]
  , frcbDefinitions :: [DefMessage TypeName]
  , frcbTypeAssignments :: [TypeMessage]
  , frcbData :: [DataUpdateMessage]
  } deriving (Show, Eq)

data ToRelayBundle
  = Trpb ToRelayProviderBundle
  | Trpr ToRelayProviderRelinquish
  | Trcb ToRelayClientBundle
  deriving Show

data FromRelayBundle
  = Frpb FromRelayProviderBundle
  | Frpeb FromRelayProviderErrorBundle
  | Frcb FromRelayClientBundle
  deriving Show

data SubOp = OpSubscribe | OpUnsubscribe deriving Show
data DefOp = OpDefine {odDef :: Definition} | OpUndefine deriving Show

data TimeSeriesDataOp =
  OpSet Time [WireValue] Interpolation | OpRemove deriving (Show)
data DataChange
  = ConstChange (Maybe Attributee) [WireValue]
  | TimeChange (Map Word32 (Maybe Attributee, TimeSeriesDataOp))
  deriving (Show)
type DataDigest = Map Path DataChange

type ChildAssignments = Map Path (Maybe Attributee, UniqList Seg)

data TrpDigest = TrpDigest
  { trpdNamespace :: Seg
  , trpdChildAssignments :: ChildAssignments
  , trpdDefinitions :: Map Seg DefOp
  , trpdData :: DataDigest
  , trpdErrors :: Map (ErrorIndex Seg) [Text]
  } deriving Show

data FrpDigest = FrpDigest
  { frpdNamespace :: Seg
  , frpdChildAssignments :: ChildAssignments
  , frpdData :: DataDigest
  } deriving Show

data FrpErrorDigest = FrpErrorDigest
  { frpedNamespace :: Seg
  , frpedErrors :: Map (ErrorIndex Seg) [Text]
  } deriving Show

data TrcDigest = TrcDigest
  { trcdTypeSubs :: Map TypeName SubOp
  , trcdDataSubs :: Map Path SubOp
  , trcdChildAssignments :: ChildAssignments
  , trcdData :: DataDigest
  } deriving Show

data FrcDigest = FrcDigest
  { frcdChildAssignments :: ChildAssignments
  , frcdDefinitions :: Map TypeName DefOp
  , frcdTypeAssignments :: Map Path (TypeName, Liberty)
  , frcdData :: DataDigest
  , frcdErrors :: Map (ErrorIndex TypeName) [Text]
  } deriving Show

newtype TrprDigest = TrprDigest {trprdNamespace :: Seg} deriving Show

data TrDigest
  = Trpd TrpDigest
  | Trprd TrprDigest
  | Trcd TrcDigest
  deriving Show

data FrDigest
  = Frpd FrpDigest
  | Frped FrpErrorDigest
  | Frcd FrcDigest
  deriving Show

trcdNamespaces :: TrcDigest -> Set Seg
trcdNamespaces (TrcDigest ts ds cas dd) =
    (Set.map tnNamespace $ Map.keysSet ts)
    <> pathKeyNss ds <> pathKeyNss cas <> pathKeyNss dd
  where
    pathKeyNss = onlyJusts . Set.map pNs . Map.keysSet
    onlyJusts = Set.map fromJust . Set.delete Nothing
    pNs (ns :</ _) = Just ns
    pNs _ = Nothing

frcdNull :: FrcDigest -> Bool
frcdNull (FrcDigest cas defs tas dd errs) =
  null cas && null defs && null tas && null dd && null errs

digestDataUpdateMessages
  :: [DataUpdateMessage] -> (ChildAssignments, DataDigest)
digestDataUpdateMessages msgs =
  let (cas, dds) = partitionEithers $ fmap procMsg msgs in
    (Map.fromList cas, Map.fromList dds)
  where
    procMsg msg = case msg of
      MsgConstSet np args att -> Right $ (np, ConstChange att args)
      MsgSet np uuid t args i att -> Right $
        (np, TimeChange (Map.singleton uuid (att, OpSet t args i)))
      MsgRemove np uuid att -> Right $
        (np, TimeChange (Map.singleton uuid (att, OpRemove)))
      MsgSetChildren np ns att -> Left $ (np, (att, ns))

produceDataUpdateMessages
  :: ChildAssignments -> DataDigest -> [DataUpdateMessage]
produceDataUpdateMessages = undefined

qualifyDefMessage :: Seg -> DefMessage Seg -> DefMessage TypeName
qualifyDefMessage ns dm = case dm of
  MsgDefine s d -> MsgDefine (TypeName ns s) d
  MsgUndefine s -> MsgUndefine $ TypeName ns s

digestDefMessages :: Ord a => [DefMessage a] -> Map a DefOp
digestDefMessages = Map.fromList . fmap procMsg
  where
    procMsg msg = case msg of
      MsgDefine a def -> (a, OpDefine def)
      MsgUndefine a -> (a, OpUndefine)

produceDefMessages :: Map a DefOp -> [DefMessage a]
produceDefMessages = undefined

digestSubMessages :: [SubMessage] -> (Map TypeName SubOp, Map Path SubOp)
digestSubMessages msgs =
  let (tss, dss) = partitionEithers $ fmap procMsg msgs in
    (Map.fromList tss, Map.fromList dss)
  where
    procMsg msg = case msg of
      MsgSubscribe p -> Right $ (p, OpSubscribe)
      MsgTypeSubscribe tn -> Left $ (tn, OpSubscribe)
      MsgUnsubscribe p -> Right $ (p, OpUnsubscribe)
      MsgTypeUnsubscribe tn -> Left $ (tn, OpUnsubscribe)

produceSubMessages :: Map TypeName SubOp -> Map Path SubOp -> [SubMessage]
produceSubMessages = undefined

digestTypeMessages :: [TypeMessage] -> Map Path (TypeName, Liberty)
digestTypeMessages = Map.fromList . fmap procMsg
  where
    procMsg (MsgAssignType p tn lib) = (p, (tn, lib))

produceTypeMessages :: Map Path TypeName -> [TypeMessage]
produceTypeMessages = undefined

digestErrMessages :: Ord a => [MsgError a] -> Map (ErrorIndex a) [Text]
digestErrMessages = foldl (Map.unionWith (<>)) mempty . fmap procMsg
  where
    procMsg (MsgError ei t) = Map.singleton ei [t]

produceErrMessages :: Map (ErrorIndex a) [Text] -> [MsgError a]
produceErrMessages = undefined

digestToRelayProviderBundle :: ToRelayProviderBundle -> TrpDigest
digestToRelayProviderBundle = undefined

produceToRelayProviderBundle :: TrpDigest -> ToRelayProviderBundle
produceToRelayProviderBundle = undefined

digestFromRelayProviderBundle :: FromRelayProviderBundle -> FrpDigest
digestFromRelayProviderBundle = undefined

produceFromRelayProviderBundle :: FrpDigest -> FromRelayProviderBundle
produceFromRelayProviderBundle = undefined

digestToRelayClientBundle :: ToRelayClientBundle -> TrcDigest
digestToRelayClientBundle = undefined

produceToRelayClientBundle :: TrcDigest -> ToRelayClientBundle
produceToRelayClientBundle = undefined

digestFromRelayClientBundle :: FromRelayClientBundle -> FrcDigest
digestFromRelayClientBundle = undefined

produceFromRelayClientBundle :: FrcDigest -> FromRelayClientBundle
produceFromRelayClientBundle = undefined

digestToRelayBundle :: ToRelayBundle -> TrDigest
digestToRelayBundle = undefined

produceFromRelayBundle :: FrDigest -> FromRelayBundle
produceFromRelayBundle = undefined

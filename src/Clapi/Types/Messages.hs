{-# OPTIONS_GHC -Wall -Wno-orphans #-}

module Clapi.Types.Messages where

import Data.Either (partitionEithers)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Text (Text)
import Data.Word (Word32)

import Clapi.Types.Base (Attributee, Site, Time, Interpolation)
import Clapi.Types.Path (Seg, NodePath, TypePath, isParentOf)
import Clapi.Types.Wire (WireValue)
import Clapi.Types.UniqList (UniqList, mkUniqList)

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

--

data TreeOp = OpAssignType TypePath | OpDelete deriving (Show)

data ConstDataOp = OpConstSet [WireValue] | OpConstClear deriving (Show)
data TimeSeriesDataOp =
  OpSet Time [WireValue] Interpolation | OpRemove | OpClear deriving (Show)
data DataChange
  = ConstChange (Maybe Attributee, ConstDataOp)
  | TimeChange (Map Word32 (Maybe Attributee, TimeSeriesDataOp))
  deriving (Show)

newtype DataDigest
  = DataDigest (Map (NodePath, Maybe Site) DataChange)
  deriving (Show)

-- NB mappend for Map prefers the original content when keys match, so we flip
-- the operands here where required:
instance Monoid DataDigest where
  mempty = DataDigest mempty
  (DataDigest m0) `mappend` (DataDigest m1) =
      DataDigest $ Map.unionWith onDc m0 m1
    where
      (TimeChange m0) `onDc` (TimeChange m1) = TimeChange (m1 <> m0)
      _ `onDc` dd = dd

type ChildAssignments = Map NodePath (Maybe Attributee, UniqList Seg)

data UpdateBundleDigest = UpdateBundleDigest
  { ubdTreeOps :: Map NodePath TreeOp
  , ubdChildAssignments :: ChildAssignments
  , ubdDataUpdates :: DataDigest
  , ubdErrors :: Map NodePath [Text]
  } deriving (Show)

treeUpdateMessageDigest :: TreeUpdateMessage -> UpdateBundleDigest
treeUpdateMessageDigest msg = UpdateBundleDigest ta mempty mempty mempty
  where
    ta = case msg of
      MsgAssignType np tp -> Map.singleton np $ OpAssignType tp
      MsgDelete np -> Map.singleton np $ OpDelete

dataUpdateMessageDigestive
  :: DataUpdateMessage -> Either ChildAssignments DataDigest
dataUpdateMessageDigestive msg =
  let dd p s = Right . DataDigest . Map.singleton (p, s) in
  case msg of
    MsgConstSet np args att site ->
      dd np site $ ConstChange (att, OpConstSet args)
    MsgConstClear np att site ->
      dd np site $ ConstChange (att, OpConstClear)
    MsgSet np uuid t args i att site ->
      dd np site $ TimeChange (Map.singleton uuid (att, OpSet t args i))
    MsgRemove np uuid att site ->
      dd np site $ TimeChange (Map.singleton uuid (att, OpRemove))
    MsgClear np uuid att site ->
      dd np site $ TimeChange (Map.singleton uuid (att, OpClear))
    MsgSetChildren np ns att ->
      Left $ Map.singleton np (att, ns)

updateBundleDumDigest :: DataUpdateMessage -> UpdateBundleDigest
updateBundleDumDigest = either
  (\ca -> UpdateBundleDigest mempty ca mempty mempty)
  (\du -> UpdateBundleDigest mempty mempty du mempty)
  . dataUpdateMessageDigestive

errorMessageDigest :: MsgError -> UpdateBundleDigest
errorMessageDigest (MsgError np text) =
  UpdateBundleDigest mempty mempty mempty $ Map.singleton np [text]

instance Monoid UpdateBundleDigest where
  mempty = UpdateBundleDigest mempty mempty mempty mempty
  (UpdateBundleDigest to0 ca0 du0 e0)
    `mappend` (UpdateBundleDigest to1 ca1 du1 e1) =
        UpdateBundleDigest treeOps (ca1 <> ca0)
          (du0 <> du1) (Map.unionWith (<>) e0 e1)
    where
      treeOps = minimiseDeletes $ to1 <> to0
      minimiseDeletes m = snd $ Map.foldlWithKey f (Nothing, mempty) m
      f (Nothing, acc) path OpDelete = (Just path, Map.insert path OpDelete acc)
      f (Just state, acc) path OpDelete
        | state `isParentOf` path = (Just state, acc)
        | otherwise = (Just path, Map.insert path OpDelete acc)
      f (s, acc) path (OpAssignType tp) = (s, Map.insert path (OpAssignType tp) acc)

digestUpdateBundle :: UpdateBundle -> UpdateBundleDigest
digestUpdateBundle (UpdateBundle errs oums) =
  let
    (tums, dums) = partitionEithers oums
  in
    mconcat $
      fmap treeUpdateMessageDigest tums ++
      fmap updateBundleDumDigest dums ++
      fmap errorMessageDigest errs

produceErrs :: Map NodePath [Text] -> [MsgError]
produceErrs es = mconcat $ (\(p, errs) -> MsgError p <$> errs) <$> Map.toList es

produceTums :: Map NodePath TreeOp -> [TreeUpdateMessage]
produceTums tas =
    foldl (\msgs (np, taOp) -> g' np taOp : msgs) [] $ Map.toList tas
  where
    g' np taOp = case taOp of
        OpAssignType tp -> MsgAssignType np tp
        OpDelete -> MsgDelete np

produceSetChildren
  :: Map NodePath (Maybe Attributee, UniqList Seg) -> [DataUpdateMessage]
produceSetChildren cas =
    (\(np, (att, segs)) -> MsgSetChildren np segs att) <$> Map.toList cas

produceDataChanges :: DataDigest -> [DataUpdateMessage]
produceDataChanges (DataDigest m) = mconcat $ uncurry i' <$> Map.toList m
  where
    i' (np, s) dc = case dc of
      ConstChange (att, op) -> pure $ case op of
        OpConstSet wvs -> MsgConstSet np wvs att s
        OpConstClear -> MsgConstClear np att s
      TimeChange m -> i'' np s <$> Map.toList m
    i'' np s (ptId, (att, op)) = case op of
      OpSet t wvs interp -> MsgSet np ptId t wvs interp att s
      OpRemove -> MsgRemove np ptId att s
      OpClear -> MsgClear np ptId att s

produceUpdateBundle :: UpdateBundleDigest -> UpdateBundle
produceUpdateBundle (UpdateBundleDigest tas cas dus es) =
    UpdateBundle (produceErrs es) (
      (Left <$> produceTums tas) ++
      (Right <$> (produceSetChildren cas ++ produceDataChanges dus)))

-- FIXME: the names/types here feel weirdly misaligned...
produceOwnerRequestBundle :: UpdateBundleDigest -> OwnerRequestBundle
produceOwnerRequestBundle (UpdateBundleDigest tas cas dus es) =
    OwnerRequestBundle (produceErrs es) (
        produceSetChildren cas ++ produceDataChanges dus)

data SubOp = OpSubscribe | OpUnsubscribe deriving Show

data RequestBundleDigest = RequestBundleDigest
  { rbdSubscriptions :: Map NodePath SubOp
  , rbdChildAssignments :: ChildAssignments
  , rbdDataUpdates :: DataDigest
  } deriving (Show)

instance Monoid RequestBundleDigest where
  mempty = RequestBundleDigest mempty mempty mempty
  (RequestBundleDigest s0 ca0 du0) `mappend` (RequestBundleDigest s1 ca1 du1) =
    RequestBundleDigest (s1 <> s0) (ca1 <> ca0) (du0 <> du1)

subMessageDigest :: SubMessage -> RequestBundleDigest
subMessageDigest msg = RequestBundleDigest s mempty mempty
  where
    s = case msg of
      (MsgSubscribe np) -> Map.singleton np OpSubscribe
      (MsgUnsubscribe np) -> Map.singleton np OpUnsubscribe

requestBundleDumDigest :: DataUpdateMessage -> RequestBundleDigest
requestBundleDumDigest = either
  (\ca -> RequestBundleDigest mempty ca mempty)
  (\du -> RequestBundleDigest mempty mempty du)
  . dataUpdateMessageDigestive

digestRequestBundle :: RequestBundle -> RequestBundleDigest
digestRequestBundle (RequestBundle subs dums) = mconcat $
  fmap subMessageDigest subs ++ fmap requestBundleDumDigest dums

-- Would only need this if using as a client
produceRequestBundle :: RequestBundleDigest -> RequestBundle
produceRequestBundle = undefined

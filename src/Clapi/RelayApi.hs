{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Clapi.RelayApi (relayApiProto, PathSegable(..)) where

import Data.Monoid
import Control.Monad (when)
import Control.Monad.Fail (MonadFail)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Word
import Control.Concurrent.MVar
import Control.Monad.Trans (lift)
import qualified Data.Map as Map

import Clapi.PerClientProto (ClientEvent(..), ServerEvent(..))
import Clapi.Types (
    ToRelayBundle(..), FromRelayBundle(FRBClient), InterpolationLimit(ILConstant),
    Interpolation(IConstant), DataUpdateMessage(..), TreeUpdateMessage(..),
    OwnerUpdateMessage, Wireable, WireValue(..), castWireValue, Time(..),
    UpdateBundle(..),
    RequestBundle(..), SubMessage(..),
    TimeStamped(..), Liberty(Cannot))
import Clapi.Types.Path (Path, Seg, pattern Root, pattern (:/), toText)
import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)
import Clapi.TH (pathq, segq)
import Clapi.NamespaceTracker (Owners)
import Clapi.TimeDelta (tdZero, getDelta, TimeDelta(..))
import Clapi.Util (strictZipWith, fmtStrictZipError)
import Clapi.Types.AssocList (mkAssocList)
import Clapi.Types.Definitions
  (TupleDefinition(..), StructDefinition(..), ArrayDefinition(..), toWireValues)
import Clapi.TextSerialisation (ttFromText)

zt :: Time
zt = Time 0 0

sdp, tdp, adp :: Path
sdp = [pathq|/api/types/base/struct|]
tdp = [pathq|/api/types/base/tuple|]
adp = [pathq|/api/types/base/array|]

staticAdd :: Path -> [WireValue] -> OwnerUpdateMessage
staticAdd p vs = Right $ MsgAdd p zt vs IConstant Nothing Nothing

staticSet :: Path -> [WireValue] -> OwnerUpdateMessage
staticSet p vs = Right $ MsgSet p zt vs IConstant Nothing Nothing

staticStructDefMsg :: Path -> Text -> [(Seg, Path)]-> OwnerUpdateMessage
staticStructDefMsg sp doc tm =
  let
    al = fromJust $ mkAssocList (fmap (\(n, p) -> (n, (p, Cannot))) tm)
  in
    staticAdd sp $ toWireValues $ StructDefinition doc al

staticTupleDefMsg :: Path -> Text -> [(Seg, Text)] -> OwnerUpdateMessage
staticTupleDefMsg p d fm = let al = fromJust $ mkAssocList $ fmap (fromJust . ttFromText) <$> fm in
    staticAdd p $ toWireValues $ TupleDefinition d al ILConstant

clRef :: Path -> WireValue
clRef = WireValue . toText

staticArrayDefMsg :: Path -> Text -> Path -> OwnerUpdateMessage
staticArrayDefMsg p d ct = staticAdd p $ toWireValues $ ArrayDefinition d ct Cannot

class PathSegable a where
    pathNameFor :: a -> Seg

relayApiProto ::
    (Ord i, PathSegable i) =>
    MVar (Owners i) ->
    i ->
    Protocol
        (ClientEvent i (TimeStamped ToRelayBundle)) (ClientEvent i ToRelayBundle)
        (ServerEvent i FromRelayBundle) (ServerEvent i FromRelayBundle)
        IO ()
relayApiProto ownerMv selfAddr =
    publishRelayApi >> subRoot >>
    steadyState mempty (Map.singleton ownSeg tdZero)
  where
    toNST = sendFwd . ClientData selfAddr
    pubUpdate = toNST . TRBOwner . UpdateBundle []
    publishRelayApi = pubUpdate
      [ Left $ MsgAssignType [pathq|/relay|] rtp
      , staticStructDefMsg rtp "topdoc" [
        ([segq|build|], btp), ([segq|self|], selfTP), ([segq|clients|], catp), ([segq|types|], ttp),
        ([segq|owners|], odp)]
      , Left $ MsgAssignType [pathq|/relay/types/types|] sdp
      , Left $ MsgAssignType [pathq|/relay/types|] ttp
      , staticStructDefMsg ttp "typedoc" [
        ([segq|relay|], sdp), ([segq|types|], sdp), ([segq|self|], tdp), ([segq|clients|], adp),
        ([segq|client_info|], tdp), ([segq|owner_info|], tdp), ([segq|owners|], adp),
        ([segq|build|], tdp)]
      , staticArrayDefMsg catp "clientsdoc" citp
      , Right $ MsgSetChildren cap [ownSeg] Nothing
      , staticAdd (cap :/ ownSeg) [WireValue $ unTimeDelta tdZero]
      , staticTupleDefMsg citp
        "Info about connected clients (clock_diff is in seconds)"
        [([segq|clock_diff|], "float")]
      , staticArrayDefMsg odp "ownersdoc" oidp
      , staticTupleDefMsg oidp "owner info" [([segq|owner|], refOf citp)]
      , Right $ MsgSetChildren oap [] Nothing
      , staticTupleDefMsg selfTP "Which client are you" [([segq|info|], refOf citp)]
      , staticAdd selfP [clRef $ cap :/ ownSeg]
      , staticTupleDefMsg btp "builddoc" [([segq|commit_hash|], "string[banana]")]
      , staticAdd [pathq|/relay/build|] [WireValue @Text "banana"]
      ]
    rtp = [pathq|/relay/types/relay|]
    btp = [pathq|/relay/types/build|]
    ttp = [pathq|/relay/types/types|]
    catp = [pathq|/relay/types/clients|]
    citp = [pathq|/relay/types/client_info|]
    cap = [pathq|/relay/clients|]
    oidp = [pathq|/relay/types/owner_info|]
    odp = [pathq|/relay/types/owners|]
    oap = [pathq|/relay/owners|]
    selfTP = [pathq|/relay/types/self|]
    selfP = [pathq|/relay/self|]
    refOf p = "ref[" <> toText p <> "]"
    ownSeg = pathNameFor selfAddr
    subRoot = toNST $ TRBClient $ RequestBundle [MsgSubscribe Root] []
    steadyState oldOwnerMap ci = waitThen (fwd oldOwnerMap ci) (rev oldOwnerMap ci)
    pubOwnerMap old new = when (Map.keys old /= Map.keys new) $ do
        let scm = Right $ MsgSetChildren oap (Map.keys new) Nothing
        let om = (cap :/) . pathNameFor <$> Map.difference new old
        pubUpdate $ scm : ((\(ownerN, refP) -> staticAdd (oap :/ ownerN) [clRef refP]) <$> Map.toList om)
    handleOwnTat oldOwnerMap _ = do
        newOwnerMap <- lift (readMVar ownerMv)
        pubOwnerMap oldOwnerMap newOwnerMap
        return newOwnerMap
    clientPov ci i (FRBClient (UpdateBundle errs oums)) = let
        theirSeg = pathNameFor i
        -- FIXME: don't really want to unTimeDelta here, as TimeDelta's support
        -- arithmetic. However, currently transform CVs kinda rightfully
        -- prevents us changing Wireable type, and TimeDelta's aren't
        -- serialisable!
        theirTime = unTimeDelta $ Map.findWithDefault
          (error "Can't rewrite message for unconnected client") theirSeg ci
        relClientInfo p = if p == selfP
            then \_ -> [clRef $ cap :/ theirSeg]
            else case p of
                (pp :/ _) -> if pp == cap
                    then either error id . transformCvs [subtract theirTime]
                    else id
                _ -> id
        mkRelative (Right (MsgAdd p t v i' a s)) = Right $ MsgAdd p t (relClientInfo p v) i' a s
        mkRelative (Right (MsgSet p t v i' a s)) = Right $ MsgSet p t (relClientInfo p v) i' a s
        mkRelative m = m
        oums' = mkRelative <$> oums
      in
        ServerData i $ FRBClient $ UpdateBundle errs oums'
    clientPov _ i ob = ServerData i ob
    fwd oldOwnerMap ci (ClientConnect cid) = sendFwd (ClientConnect cid) >> pubUpdate uMsgs >> steadyState oldOwnerMap ci'
      where
        cSeg = pathNameFor cid
        ci' = Map.insert cSeg tdZero ci
        uMsgs =
          [ Right $ MsgSetChildren cap (Map.keys ci') Nothing
          , staticAdd (cap :/ cSeg) [WireValue $ unTimeDelta tdZero]
          ]
    fwd oldOwnerMap ci (ClientData cid (TimeStamped (theirTime, trb))) = do
        let cSeg = pathNameFor cid
        d <- lift $ getDelta theirTime
        let ci' = Map.insert cSeg d ci
        pubUpdate [ staticSet (cap :/ cSeg) [WireValue $ unTimeDelta d] ]
        sendFwd (ClientData cid trb)
        steadyState oldOwnerMap ci'
    fwd oldOwnerMap ci (ClientDisconnect cid) = sendFwd (ClientDisconnect cid) >> removeClient oldOwnerMap ci cid
    rev oldOwnerMap ci (ServerData i frb) = do
        newOwnerMap <- if selfAddr == i
          then handleOwnTat oldOwnerMap frb
          else sendRev (clientPov ci i frb) >> return oldOwnerMap
        steadyState newOwnerMap ci
    rev oldOwnerMap ci b@(ServerDisconnect cid) = sendRev b >> removeClient oldOwnerMap ci cid
    removeClient oldOwnerMap ci cid =
        pubUpdate [ Right $ MsgSetChildren cap (Map.keys ci') Nothing ] >>
        steadyState oldOwnerMap ci'
      where
        ci' = Map.delete (pathNameFor cid) ci

transformCvs
  :: (Wireable a, MonadFail m) => [a -> a] -> [WireValue] -> m [WireValue]
transformCvs ss cvs =
  fmtStrictZipError "functions" "wire values"
    (strictZipWith (\f wv -> WireValue . f <$> castWireValue wv) ss cvs)
  >>= sequence

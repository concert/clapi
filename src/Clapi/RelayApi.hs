{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Clapi.RelayApi (relayApiProto, PathSegable(..)) where

import Data.Monoid
import Control.Monad (when)
import Control.Monad.Fail (MonadFail)
import Data.Text (Text)
import Data.Word
import Control.Concurrent.MVar
import Control.Monad.Trans (lift)
import qualified Data.Map as Map

import Clapi.PerClientProto (ClientEvent(..), ServerEvent(..))
import Clapi.Types (
    ToRelayBundle(..), FromRelayBundle(FRBClient), InterpolationType(ITConstant),
    Interpolation(IConstant), DataUpdateMessage(..), TreeUpdateMessage(..),
    OwnerUpdateMessage, Wireable, WireValue(..), castWireValue, Time(..),
    UpdateBundle(..),
    RequestBundle(..), SubMessage(..),
    TimeStamped(..), Liberty(Cannot))
import Clapi.Types.Path (Path, Seg, pattern Root, pattern (:/), toText)
import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)
import Clapi.TH (pathq)
import Clapi.NamespaceTracker (Owners)
import Clapi.TimeDelta (tdZero, getDelta, TimeDelta(..))
import Clapi.Util (strictZipWith, fmtStrictZipError)

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

structDefMsg :: Path -> Text -> [(Text, Path)]-> OwnerUpdateMessage
structDefMsg sp doc tm = let
    liberties = replicate (length tm) Cannot
    targs =
      [ WireValue doc
      , WireValue $ map fst tm
      , WireValue $ map (toText . snd) tm
      , WireValue @[Word8] $ fmap (fromIntegral . fromEnum) liberties]
  in
    staticAdd sp targs

tupleDefMsg :: Path -> Text -> [(Text, Text)] -> OwnerUpdateMessage
tupleDefMsg p d fm = let
    targs =
      [ WireValue d
      , WireValue $ map fst fm
      , WireValue $ map snd fm
      , WireValue @[Word8] $ fmap (fromIntegral . fromEnum) [ITConstant]]
  in
    staticAdd p targs

clRef :: Path -> WireValue
clRef = WireValue . toText

arrayDefMsg :: Path -> Text -> Path -> OwnerUpdateMessage
arrayDefMsg p d ct = staticAdd p
  [WireValue d, clRef ct, WireValue @Word8 $ fromIntegral $ fromEnum $ Cannot]

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
      , structDefMsg rtp "topdoc" [
        ("build", btp), ("self", selfTP), ("clients", catp), ("types", ttp),
        ("owners", odp)]
      , Left $ MsgAssignType [pathq|/relay/types/types|] sdp
      , Left $ MsgAssignType [pathq|/relay/types|] ttp
      , structDefMsg ttp "typedoc" [
        ("relay", sdp), ("types", sdp), ("self", tdp), ("clients", adp),
        ("client_info", tdp), ("owner_info", tdp), ("owners", adp),
        ("build", tdp)]
      , arrayDefMsg catp "clientsdoc" citp
      , Right $ MsgSetChildren cap [ownSeg] Nothing
      , staticAdd (cap :/ ownSeg) [WireValue $ unTimeDelta tdZero]
      , tupleDefMsg citp
        "Info about connected clients (clock_diff is in seconds)"
        [("clock_diff", "float")]
      , arrayDefMsg odp "ownersdoc" oidp
      , tupleDefMsg oidp "owner info" [("owner", refOf citp)]
      , Right $ MsgSetChildren oap [] Nothing
      , tupleDefMsg selfTP "Which client are you" [("info", refOf citp)]
      , staticAdd selfP [clRef $ cap :/ ownSeg]
      , tupleDefMsg btp "builddoc" [("commit_hash", "string[banana]")]
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

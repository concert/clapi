{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Clapi.RelayApi (relayApiProto, PathSegmenty(..)) where
import Data.Monoid
import Control.Monad (forever, when)
import Data.Text (Text, pack)
import qualified Data.List as List
import Control.Concurrent.MVar
import Control.Monad.Trans (lift)
import qualified Data.Map as Map

import Clapi.Path (Path, (+|), Name, root)
import Path.Parsing (toText)
import Clapi.PerClientProto (ClientEvent(..), ServerEvent(..))
import Clapi.Types (
    ToRelayBundle(..), FromRelayBundle, InterpolationType(ITConstant),
    Interpolation(IConstant), DataUpdateMessage(..), TreeUpdateMessage(..),
    OwnerUpdateMessage(..), toClapiValue, Time(..), UpdateBundle(..),
    ClapiValue(ClString), Enumerated(..), RequestBundle(..), SubMessage(..),
    TimeStamped(..))
import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)
import Clapi.Valuespace (Liberty(Cannot))
import Clapi.PathQ (pathq)
import Clapi.NamespaceTracker (Owners)
import Clapi.TimeDelta (tdZero, getDelta, tdClVal)

zt = Time 0 0

sdp = [pathq|/api/types/base/struct|]
tdp = [pathq|/api/types/base/tuple|]
adp = [pathq|/api/types/base/array|]

staticAdd :: Path -> [ClapiValue] -> OwnerUpdateMessage
staticAdd p vs = Right $ UMsgAdd p zt vs IConstant Nothing Nothing

staticSet :: Path -> [ClapiValue] -> OwnerUpdateMessage
staticSet p vs = Right $ UMsgSet p zt vs IConstant Nothing Nothing

structDefMsg :: Path -> Text -> [(Text, Path)]-> OwnerUpdateMessage
structDefMsg sp doc tm = let
    liberties = replicate (length tm) $ Enumerated Cannot
    targs =
      [ ClString doc
      , toClapiValue $ map fst tm
      , toClapiValue $ map (toText . snd) tm
      , toClapiValue liberties]
  in
    staticAdd sp targs

tupleDefMsg :: Path -> Text -> [(Text, Text)] -> OwnerUpdateMessage
tupleDefMsg p d fm = let
    targs =
      [ ClString d
      , toClapiValue $ map fst fm
      , toClapiValue $ map snd fm
      , toClapiValue [Enumerated ITConstant]]
  in
    staticAdd p targs

clRef :: Path -> ClapiValue
clRef = ClString . toText

arrayDefMsg :: Path -> Text -> Path -> OwnerUpdateMessage
arrayDefMsg p d ct = staticAdd p [ClString d, clRef ct, toClapiValue $ Enumerated Cannot]

class PathSegmenty a where
    pathSegmentFor :: a -> Name

relayApiProto ::
    (Ord i, PathSegmenty i) =>
    MVar (Owners i) ->
    i ->
    Protocol
        (ClientEvent i (TimeStamped ToRelayBundle)) (ClientEvent i ToRelayBundle)
        (ServerEvent i FromRelayBundle) (ServerEvent i FromRelayBundle)
        IO ()
relayApiProto ownerMv selfAddr =
    publishRelayApi >> subRoot >>
    steadyState mempty (Map.fromList [(ownSeg, tdZero)])
  where
    toNST = sendFwd . ClientData selfAddr
    pubUpdate = toNST . TRBOwner . UpdateBundle []
    publishRelayApi = pubUpdate
      [ Left $ UMsgAssignType [pathq|/relay|] rtp
      , structDefMsg rtp "topdoc" [("build", btp), ("clients", catp), ("types", ttp), ("owners", odp)]
      , Left $ UMsgAssignType [pathq|/relay/types/types|] sdp
      , Left $ UMsgAssignType [pathq|/relay/types|] ttp
      , structDefMsg ttp "typedoc" [
        ("relay", sdp), ("types", sdp), ("clients", adp), ("client_info", tdp),
        ("owner_info", tdp), ("owners", adp), ("build", tdp)]
      , arrayDefMsg catp "clientsdoc" citp
      , Right $ UMsgSetChildren cap [ownSeg] Nothing
      , staticAdd (cap +| ownSeg) [tdClVal tdZero]
      , tupleDefMsg citp "client info" [("clock_diff", "float")]
      , arrayDefMsg odp "ownersdoc" oidp
      , tupleDefMsg oidp "owner info" [("owner", refOf citp)]
      , Right $ UMsgSetChildren oap [] Nothing
      , tupleDefMsg btp "builddoc" [("commit_hash", "string[banana]")]
      , staticAdd [pathq|/relay/build|] [ClString "banana"]
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
    refOf p = "ref[" <> toText p <> "]"
    ownSeg = pathSegmentFor selfAddr
    subRoot = toNST $ TRBClient $ RequestBundle [UMsgSubscribe root] []
    steadyState oldOwnerMap ci = waitThen (fwd oldOwnerMap ci) (rev oldOwnerMap ci)
    pubOwnerMap old new = when (Map.keys old /= Map.keys new) $ do
        let scm = Right $ UMsgSetChildren oap (Map.keys new) Nothing
        let om = (cap +|) . pathSegmentFor <$> Map.difference new old
        pubUpdate $ scm : ((\(ownerN, refP) -> staticAdd (oap +| ownerN) [clRef refP]) <$> Map.toList om)
    handleOwnTat oldOwnerMap _ = do
        newOwnerMap <- lift (readMVar ownerMv)
        pubOwnerMap oldOwnerMap newOwnerMap
        return newOwnerMap
    fwd oldOwnerMap ci (ClientConnect cid) = sendFwd (ClientConnect cid) >> pubUpdate uMsgs >> steadyState oldOwnerMap ci'
      where
        cSeg = pathSegmentFor cid
        ci' = Map.insert cSeg tdZero ci
        uMsgs =
          [ Right $ UMsgSetChildren cap (Map.keys ci') Nothing
          , staticAdd (cap +| cSeg) [tdClVal tdZero]
          ]
    fwd oldOwnerMap ci (ClientData cid (TimeStamped (theirTime, trb))) = do
        let cSeg = pathSegmentFor cid
        d <- lift $ getDelta theirTime
        let ci' = Map.insert cSeg d ci
        pubUpdate [ staticSet (cap +| cSeg) [tdClVal d] ]
        sendFwd (ClientData cid trb)
        steadyState oldOwnerMap ci'
    fwd oldOwnerMap ci (ClientDisconnect cid) = sendFwd (ClientDisconnect cid) >> removeClient oldOwnerMap ci cid
    rev oldOwnerMap ci b@(ServerData i ob) = do
        newOwnerMap <- if selfAddr == i then handleOwnTat oldOwnerMap ob else sendRev b >> return oldOwnerMap
        steadyState newOwnerMap ci
    rev oldOwnerMap ci b@(ServerDisconnect cid) = sendRev b >> removeClient oldOwnerMap ci cid
    removeClient oldOwnerMap ci cid =
        pubUpdate [ Right $ UMsgSetChildren cap (Map.keys ci') Nothing ] >>
        steadyState oldOwnerMap ci'
      where
        ci' = Map.delete (pathSegmentFor cid) ci

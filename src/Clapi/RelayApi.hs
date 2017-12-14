{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Clapi.RelayApi (relayApiProto) where
import Control.Monad (forever)
import Data.Text (Text)

import Clapi.Path (Path)
import Path.Parsing (toText)
import Clapi.PerClientProto (ClientEvent(ClientData), ServerEvent)
import Clapi.Types (ToRelayBundle(..), FromRelayBundle, InterpolationType(ITConstant), Interpolation(IConstant), DataUpdateMessage(..), TreeUpdateMessage(..), OwnerUpdateMessage(..), toClapiValue, Time(..), UpdateBundle(..), ClapiValue(ClString), Enumerated(..))
import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)
import Clapi.Valuespace (Liberty(Cannot))
import Clapi.PathQ (pathq)

zt = Time 0 0

sdp = [pathq|/api/types/base/struct|]
tdp = [pathq|/api/types/base/tuple|]
adp = [pathq|/api/types/base/array|]

staticAdd :: Path -> [ClapiValue] -> OwnerUpdateMessage
staticAdd p vs = Right $ UMsgAdd p zt vs IConstant Nothing Nothing

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

arrayDefMsg :: Path -> Text -> Path -> OwnerUpdateMessage
arrayDefMsg p d ct = staticAdd p [ClString d, ClString $ toText ct, toClapiValue $ Enumerated Cannot]

relayApiProto ::
    (Ord i) =>
    i ->
    Protocol
        (ClientEvent i ToRelayBundle) (ClientEvent i ToRelayBundle)
        (ServerEvent i FromRelayBundle) (ServerEvent i FromRelayBundle)
        IO ()
relayApiProto selfAddr = publishRelayApi >> cat
  where
    publishRelayApi = sendFwd $ ClientData selfAddr $ TRBOwner $ UpdateBundle [] $
      [ Left $ UMsgAssignType [pathq|/relay|] rtp
      , structDefMsg rtp "topdoc" [("build", btp), ("clients", catp), ("types", ttp)]
      , Left $ UMsgAssignType [pathq|/relay/types/types|] sdp
      , Left $ UMsgAssignType [pathq|/relay/types|] ttp
      , structDefMsg ttp "typedoc" [("relay", sdp), ("types", sdp), ("clients", adp), ("client_info", tdp), ("build", tdp)]
      , arrayDefMsg catp "clientsdoc" citp
      , Right $ UMsgSetChildren [pathq|/relay/clients|] ["relay"] Nothing  -- FIXME: should be i
      , staticAdd [pathq|/relay/clients/relay|] []
      , tupleDefMsg citp "client info" []
      , tupleDefMsg btp "builddoc" [("commit_hash", "string[banana]")]
      , staticAdd [pathq|/relay/build|] [ClString "banana"]
      ]
    rtp = [pathq|/relay/types/relay|]
    btp = [pathq|/relay/types/build|]
    ttp = [pathq|/relay/types/types|]
    catp = [pathq|/relay/types/clients|]
    citp = [pathq|/relay/types/client_info|]
    cat = forever $ waitThen sendFwd sendRev

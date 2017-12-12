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

staticAdd :: Path -> [ClapiValue] -> OwnerUpdateMessage
staticAdd p vs = Right $ UMsgAdd p zt vs IConstant Nothing Nothing

structDefMsgs :: Path -> Text -> [(Text, Path)]-> [OwnerUpdateMessage]
structDefMsgs sp doc tm = let
    liberties = replicate (length tm) $ Enumerated Cannot
    targs =
      [ ClString doc
      , toClapiValue $ map fst tm
      , toClapiValue $ map (toText . snd) tm
      , toClapiValue liberties]
  in
    [ Left $ UMsgAssignType sp sdp
    , staticAdd sp targs]

tupleDefMsgs :: Path -> Text -> [(Text, Text)] -> [OwnerUpdateMessage]
tupleDefMsgs p d fm = let
    targs =
      [ ClString d
      , toClapiValue $ map fst fm
      , toClapiValue $ map snd fm
      , toClapiValue [Enumerated ITConstant]]
  in
    [ Left $ UMsgAssignType p tdp
    , staticAdd p targs]

relayApiProto ::
    (Ord i) =>
    i ->
    Protocol
        (ClientEvent i ToRelayBundle) (ClientEvent i ToRelayBundle)
        (ServerEvent i FromRelayBundle) (ServerEvent i FromRelayBundle)
        IO ()
relayApiProto selfAddr = publishRelayApi >> cat
  where
    publishRelayApi = sendFwd $ ClientData selfAddr $ TRBOwner $ UpdateBundle [] $ concat
      [ [Left $ UMsgAssignType [pathq|/relay|] rtp]
      , structDefMsgs rtp "topdoc" [("build", btp), ("types", ttp)]
      , [Left $ UMsgAssignType [pathq|/relay/types|] ttp]
      , structDefMsgs ttp "typedoc" [("relay", sdp), ("types", sdp), ("build", tdp)]
      , tupleDefMsgs btp "builddoc" [("commit_hash", "string[banana]")]
      , initialData
      ]
    initialData =
      [ staticAdd [pathq|/relay/build|] [ClString "banana"]
      , Left $ UMsgAssignType [pathq|/relay/build|] btp
      ]
    rtp = [pathq|/relay/types/relay|]
    btp = [pathq|/relay/types/build|]
    ttp = [pathq|/relay/types/types|]
    cat = forever $ waitThen sendFwd sendRev

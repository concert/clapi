{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Clapi.RelayApi (relayApiProto, PathSegable(..)) where

import Data.Text (Text)
import Control.Monad.Trans (lift)
import Data.Map (Map)
import qualified Data.Map as Map

import Clapi.PerClientProto (ClientEvent(..), ServerEvent(..))
import Clapi.Types
  ( TrDigest(..), TrpDigest(..), FrDigest(..), WireValue(..)
  , TimeStamped(..), Liberty(Cannot))
import Clapi.Types.AssocList
  (alEmpty, alSingleton, alFromMap, alFmapWithKey, alFromList)
import Clapi.Types.Base (InterpolationLimit(ILUninterpolated))
import Clapi.Types.Definitions (tupleDef, structDef, arrayDef)
import Clapi.Types.Digests
  (DefOp(OpDefine), DataChange(..), TrcDigest(..), SubOp(..), FrcDigest(..))
import Clapi.Types.Path (Seg, TypeName(..), pattern Root, pattern (:/))
import qualified Clapi.Types.Path as Path
import Clapi.Types.Tree (unbounded, ttString, ttFloat, ttRef)
import Clapi.Types.Wire (castWireValue)
import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)
import Clapi.TH (pathq, segq)
import Clapi.TimeDelta (tdZero, getDelta, TimeDelta(..))

class PathSegable a where
    pathNameFor :: a -> Seg

relayApiProto ::
    (Ord i, PathSegable i) =>
    IO (Map Seg i) ->
    i ->
    Protocol
        (ClientEvent i (TimeStamped TrDigest)) (ClientEvent i TrDigest)
        (ServerEvent i FrDigest) (ServerEvent i FrDigest)
        IO ()
relayApiProto getOwners selfAddr =
    publishRelayApi >> subRoot >> steadyState mempty
  where
    publishRelayApi = sendFwd $ ClientData selfAddr $ Trpd $ TrpDigest
      rns
      mempty
      (Map.fromList $ fmap OpDefine <$>
        [ ([segq|build|], tupleDef "builddoc"
             (alSingleton [segq|commit_hash|] $ ttString "banana")
             ILUninterpolated)
        , ([segq|client_info|], tupleDef
             "Info about connected clients (clock_diff is in seconds)"
             (alSingleton [segq|clock_diff|] $ ttFloat unbounded)
             ILUninterpolated)
        , ([segq|clients|], arrayDef "clientsdoc"
             (TypeName rns [segq|client_info|]) Cannot)
        , ([segq|owner_info|], tupleDef "owner info"
             (alSingleton [segq|owner|]
               $ ttRef $ TypeName rns [segq|client_info|])
             ILUninterpolated)
        , ([segq|owners|], arrayDef "ownersdoc"
             (TypeName rns [segq|owner_info|]) Cannot)
        , ([segq|self|], tupleDef "Which client you are"
             (alSingleton [segq|info|]
               $ ttRef $ TypeName rns [segq|client_info|])
             ILUninterpolated)
        , ([segq|relay|], structDef "topdoc" $ staticAl
          [ ([segq|build|], (TypeName rns [segq|build|], Cannot))
          , ([segq|clients|], (TypeName rns [segq|clients|], Cannot))
          , ([segq|owners|], (TypeName rns [segq|owners|], Cannot))
          , ([segq|self|], (TypeName rns [segq|self|], Cannot))])
        ])
      (alFromList
        [ ([pathq|/build|], ConstChange Nothing [WireValue @Text "banana"])
        , ([pathq|/self|], ConstChange Nothing [
             WireValue $ Path.toText selfClientPath])
        , (selfClientPath, ConstChange Nothing [WireValue @Float 0.0])
        , ([pathq|/owners|], InitChange Nothing)
        ])
      mempty
    rns = [segq|relay|]
    selfSeg = pathNameFor selfAddr
    selfClientPath = Root :/ [segq|clients|] :/ selfSeg
    staticAl = alFromMap . Map.fromList
    subRoot = sendFwd $ ClientData selfAddr $ Trcd $ TrcDigest
      mempty (Map.singleton [pathq|/|] OpSubscribe) mempty alEmpty
    steadyState timingMap = waitThen fwd rev
      where
        fwd ce = case ce of
          ClientConnect cAddr ->
            let
              cSeg = pathNameFor cAddr
              timingMap' = Map.insert cSeg tdZero timingMap
            in do
              sendFwd (ClientConnect cAddr)
              pubUpdate
                (alSingleton ([pathq|/clients|] :/ cSeg)
                  $ ConstChange Nothing [WireValue $ unTimeDelta tdZero])
              steadyState timingMap'
          ClientData cAddr (TimeStamped (theirTime, d)) -> do
            let cSeg = pathNameFor cAddr
            -- FIXME: this delta thing should probably be in the per client
            -- pipeline, it'd be less jittery and tidy this up
            delta <- lift $ getDelta theirTime
            let timingMap' = Map.insert cSeg delta timingMap
            pubUpdate (alSingleton ([pathq|/clients|] :/ cSeg)
              $ ConstChange Nothing [WireValue $ unTimeDelta delta])
            sendFwd $ ClientData cAddr d
            steadyState timingMap'
          ClientDisconnect cAddr ->
            sendFwd (ClientDisconnect cAddr) >> removeClient cAddr
        removeClient cAddr =
          let
            cseg = pathNameFor cAddr
            timingMap' = Map.delete cseg timingMap
          in do
            pubUpdate
              (alSingleton ([pathq|/clients|] :/ cseg) $ DeleteChange Nothing)
            steadyState timingMap'
        pubUpdate dd = sendFwd $ ClientData selfAddr $ Trpd $ TrpDigest
          rns mempty mempty dd mempty
        rev se = case se of
          ServerData cAddr d -> if cAddr == selfAddr
            then handleNsChange
            else do
              case d of
                Frcd (FrcDigest reords defs tas dd errs) ->
                  sendRev $ ServerData cAddr $ Frcd
                  $ FrcDigest reords defs tas (viewAs cAddr dd) errs
                _ -> sendRev se
              steadyState timingMap
          ServerDisconnect cAddr ->
            sendRev (ServerDisconnect cAddr) >> removeClient cAddr
        handleNsChange = do
          owners <- fmap pathNameFor <$> lift getOwners
          pubUpdate
            (alFromMap $ Map.mapKeys toOwnerPath $ toSetRefOp <$> owners)
        toOwnerPath s = [pathq|/owners|] :/ s
        toSetRefOp ns = ConstChange Nothing [
          WireValue $ Path.toText $ [pathq|/clients|] :/ ns]
        viewAs i dd =
          let
            theirSeg = pathNameFor i
            theirTime = unTimeDelta $ Map.findWithDefault
              (error "Can't rewrite message for unconnected client") theirSeg
              timingMap
            alterTime (ConstChange att [wv]) = ConstChange att $ pure
              $ WireValue $ subtract theirTime $ either error id
              $ castWireValue wv
            alterTime _ = error "Weird data back out of VS"
            fiddleDataChanges p dc
              | p `Path.isChildOf` [pathq|/relay/clients|] = alterTime dc
              | p == [pathq|/relay/self|] = toSetRefOp theirSeg
              | otherwise = dc
          in
            alFmapWithKey fiddleDataChanges dd

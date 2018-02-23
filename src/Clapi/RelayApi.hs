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
import Clapi.Types.SequenceOps (SequenceOp(..))
import Clapi.Types.Path (Seg, TypeName(..), pattern Root, pattern (:/), pattern (:</))
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
    i ->
    Protocol
        (ClientEvent i (TimeStamped TrDigest)) (ClientEvent i TrDigest)
        (ServerEvent i FrDigest) (Either (Map Seg i) (ServerEvent i FrDigest))
        IO ()
relayApiProto selfAddr =
    publishRelayApi >> steadyState mempty mempty
  where
    publishRelayApi = sendFwd $ ClientData selfAddr $ Trpd $ TrpDigest
      rns
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
             WireValue $ Path.toText $ selfSeg :</ selfClientPath])
        , (selfClientPath, ConstChange Nothing [WireValue @Float 0.0])
        ])
      (Map.singleton Root $ Map.singleton [segq|owners|] $
        (Nothing, SoPresentAfter $ Just [segq|clients|]))
      mempty
    rns = [segq|relay|]
    selfSeg = pathNameFor selfAddr
    selfClientPath = Root :/ [segq|clients|] :/ selfSeg
    staticAl = alFromMap . Map.fromList
    steadyState timingMap ownerMap = waitThen fwd rev
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
                mempty
              steadyState timingMap' ownerMap
          ClientData cAddr (TimeStamped (theirTime, d)) -> do
            let cSeg = pathNameFor cAddr
            -- FIXME: this delta thing should probably be in the per client
            -- pipeline, it'd be less jittery and tidy this up
            delta <- lift $ getDelta theirTime
            let timingMap' = Map.insert cSeg delta timingMap
            pubUpdate (alSingleton ([pathq|/clients|] :/ cSeg)
              $ ConstChange Nothing [WireValue $ unTimeDelta delta])
              mempty
            sendFwd $ ClientData cAddr d
            steadyState timingMap' ownerMap
          ClientDisconnect cAddr ->
            sendFwd (ClientDisconnect cAddr) >> removeClient cAddr
        removeClient cAddr =
          let
            cSeg = pathNameFor cAddr
            timingMap' = Map.delete cSeg timingMap
            -- FIXME: This feels a bit like reimplementing some of the NST
            ownerMap' = Map.filter (/= cSeg) ownerMap
            (dd, cops) = ownerChangeInfo ownerMap'
          in do
            pubUpdate dd $ Map.insert [pathq|/clients|]
              (Map.singleton cSeg (Nothing, SoAbsent)) cops
            steadyState timingMap' ownerMap'
        pubUpdate dd co = sendFwd $ ClientData selfAddr $ Trpd $ TrpDigest
          rns mempty dd co mempty
        rev (Left ownerAddrs) = do
          let ownerMap' = pathNameFor <$> ownerAddrs
          uncurry pubUpdate $ ownerChangeInfo ownerMap'
          steadyState timingMap ownerMap'
        rev (Right se) = do
          case se of
            ServerData cAddr d ->
              case d of
                Frcd frcd ->
                  sendRev $ ServerData cAddr $ Frcd
                  $ frcd {frcdData = viewAs cAddr $ frcdData frcd}
                _ -> sendRev se
            _ -> sendRev se
          steadyState timingMap ownerMap
        ownerChangeInfo ownerMap' =
            ( alFromMap $ Map.mapKeys toOwnerPath $ toSetRefOp <$> ownerMap'
            , Map.singleton [pathq|/owners|] $
                (const (Nothing, SoAbsent)) <$>
                  ownerMap `Map.difference` ownerMap')
        toOwnerPath s = [pathq|/owners|] :/ s
        toSetRefOp ns = ConstChange Nothing [
          WireValue $ Path.toText $ Root :/ selfSeg :/ [segq|clients|] :/ ns]
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
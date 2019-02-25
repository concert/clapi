{-# LANGUAGE
    DataKinds
  , GADTs
  , OverloadedStrings
#-}

module Clapi.RelayApi (relayApiProto, PathSegable(..)) where

import Control.Monad (void)
import Control.Monad.Trans (lift)
import Data.Bifunctor (bimap)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tagged (Tagged(..))
import qualified Data.Text as Text

import Clapi.PerClientProto (ClientEvent(..), ServerEvent(..))
import Clapi.Types (WireValue(..), TimeStamped(..), Editability(..))
import qualified Clapi.Types.AssocList as AL
import Clapi.Types.Definitions (tupleDef, structDef, arrayDef)
import Clapi.Types.Digests
  ( TrDigest(..), FrDigest(..), SomeTrDigest(..), SomeFrDigest(..)
  , trpdEmpty, OriginatorRole(..), DigestAction(..)
  , DefOp(OpDefine), DataChange(..), DataDigest, ContOps)
import Clapi.Types.SequenceOps (SequenceOp(..))
import Clapi.Types.Path (Seg, pattern Root, pattern (:/), Namespace(..))
import qualified Clapi.Types.Path as Path
import Clapi.Types.Tree (unbounded, ttString, ttFloat, ttRef)
import Clapi.Types.Wire (WireType(..), SomeWireValue(..), someWireable, someWv)
import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)
import Clapi.TH (pathq, nameq)
import Clapi.TimeDelta (tdZero, getDelta, TimeDelta(..))

class PathSegable a where
    pathNameFor :: a -> Seg

dn :: Name
dn = [nameq|display_name|]

relayApiProto ::
    forall i. (Ord i, PathSegable i) =>
    i ->
    Protocol
        (ClientEvent i (TimeStamped SomeTrDigest))
        (ClientEvent i SomeTrDigest)
        (ServerEvent i SomeFrDigest)
        (Either (Map Namespace i) (ServerEvent i SomeFrDigest))
        IO ()
relayApiProto selfAddr =
    publishRelayApi >> steadyState mempty mempty
  where
    publishRelayApi = sendFwd $ ClientData selfAddr $ SomeTrDigest $ Trpd
      rns
      mempty
      (Map.fromList $ bimap Tagged OpDefine <$>
        [ ([nameq|build|], tupleDef "builddoc"
             (AL.singleton [nameq|commit_hash|] $ ttString "banana")
             Nothing)
        , (clock_diff, tupleDef
             "The difference between two clocks, in seconds"
             (AL.singleton [nameq|seconds|] $ ttFloat unbounded)
             Nothing)
        , (dn, tupleDef
             "A human-readable name for a struct or array element"
             (AL.singleton [nameq|name|] $ ttString "")
             Nothing)
        , ([nameq|client_info|], structDef
             "Info about a single connected client" $ staticAl
             [ (dn, (Tagged dn, Editable))
             , (clock_diff, (Tagged clock_diff, ReadOnly))
             ])
        , ([nameq|clients|], arrayDef "Info about the connected clients"
             Nothing (Tagged [nameq|client_info|]) ReadOnly)
        , ([nameq|owner_info|], tupleDef "owner info"
             (AL.singleton [nameq|owner|]
               -- FIXME: want to make Ref's Name tagged...
               $ ttRef [nameq|client_info|])
             Nothing)
        , ([nameq|owners|], arrayDef "ownersdoc"
             Nothing (Tagged [nameq|owner_info|]) ReadOnly)
        , ([nameq|self|], tupleDef "Which client you are"
             (AL.singleton [nameq|info|] $ ttRef [nameq|client_info|])
             Nothing)
        , ([nameq|relay|], structDef "topdoc" $ staticAl
          [ ([nameq|build|], (Tagged [nameq|build|], ReadOnly))
          , ([nameq|clients|], (Tagged [nameq|clients|], ReadOnly))
          , ([nameq|owners|], (Tagged [nameq|owners|], ReadOnly))
          , ([nameq|self|], (Tagged [nameq|self|], ReadOnly))])
        ])
      (AL.fromList
        [ ([pathq|/build|], ConstChange Nothing [someWv WtString "banana"])
        , ([pathq|/self|], ConstChange Nothing [
             someWireable $ Path.toText Path.unSeg selfClientPath])
        , ( selfClientPath :/ clock_diff
          , ConstChange Nothing [someWv WtFloat 0.0])
        , ( selfClientPath :/ dn
          , ConstChange Nothing [someWv WtString "Relay"])
        ])
      mempty
      mempty
    rns = Namespace [nameq|relay|]
    clock_diff = [nameq|clock_diff|]
    selfName = pathNameFor selfAddr
    selfClientPath = Root :/ [nameq|clients|] :/ selfName
    staticAl = AL.fromMap . Map.fromList
    steadyState
      :: Map i TimeDelta -> Map Namespace Seg -> Protocol
            (ClientEvent i (TimeStamped SomeTrDigest))
            (ClientEvent i SomeTrDigest)
            (ServerEvent i SomeFrDigest)
            (Either (Map Namespace i) (ServerEvent i SomeFrDigest))
            IO ()
    steadyState timingMap ownerMap = waitThen fwd rev
      where
        fwd ce = case ce of
          ClientConnect displayName cAddr ->
            let
              cSeg = pathNameFor cAddr
              timingMap' = Map.insert cAddr tdZero timingMap
            in do
              sendFwd (ClientConnect displayName cAddr)
              pubUpdate (AL.fromList
                [ ( [pathq|/clients|] :/ cSeg :/ clock_diff
                  , ConstChange Nothing [someWireable $ unTimeDelta tdZero])
                , ( [pathq|/clients|] :/ cSeg :/ dn
                  , ConstChange Nothing [someWireable $ Text.pack displayName])
                ])
                mempty
              steadyState timingMap' ownerMap
          ClientData cAddr (TimeStamped (theirTime, d)) -> do
            let cSeg = pathNameFor cAddr
            -- FIXME: this delta thing should probably be in the per client
            -- pipeline, it'd be less jittery and tidy this up
            delta <- lift $ getDelta theirTime
            let timingMap' = Map.insert cAddr delta timingMap
            pubUpdate (AL.singleton ([pathq|/clients|] :/ cName :/ clock_diff)
              $ ConstChange Nothing [someWireable $ unTimeDelta delta])
              mempty
            sendFwd $ ClientData cAddr d
            steadyState timingMap' ownerMap
          ClientDisconnect cAddr ->
            sendFwd (ClientDisconnect cAddr) >> removeClient cAddr
        removeClient cAddr =
          let
            cSeg = pathNameFor cAddr
            timingMap' = Map.delete cAddr timingMap
            -- FIXME: This feels a bit like reimplementing some of the NST
            ownerMap' = Map.filter (/= cSeg) ownerMap
            (dd, cops) = ownerChangeInfo ownerMap'
          in do
            pubUpdate dd $ Map.insert [pathq|/clients|]
              (Map.singleton cSeg (Nothing, SoAbsent)) cops
            steadyState timingMap' ownerMap'
        pubUpdate dd co = sendFwd $ ClientData selfAddr $ SomeTrDigest $ Trpd
          rns mempty mempty dd co mempty
        rev (Left ownerAddrs) = do
          let ownerMap' = pathNameFor <$> ownerAddrs
          if elem selfAddr $ Map.elems ownerAddrs
            then do
              uncurry pubUpdate $ ownerChangeInfo ownerMap'
              steadyState timingMap ownerMap'
            else
              -- The relay API did something invalid and got kicked out
              return ()
        rev (Right se) = do
          case se of
            ServerData cAddr sd@(SomeFrDigest d) ->
              case d of
                Frcud {} ->
                  sendRev $ ServerData cAddr $ SomeFrDigest
                  $ d {frcudData = if frcudNs d == rns
                    then viewAs cAddr $ frcudData d
                    else frcudData d}
                Frcrd {} -> void $ sequence $ Map.mapWithKey
                  (\addr _ -> sendRev $ ServerData addr sd) timingMap
                Frpd {} -> if frpdNs d == rns
                  then handleApiRequest d
                  else sendRev se
                _ -> sendRev se
            _ -> sendRev se
          steadyState timingMap ownerMap
        ownerChangeInfo :: Map Namespace Seg -> (DataDigest, ContOps args)
        ownerChangeInfo ownerMap' =
            ( AL.fromMap $ Map.mapKeys toOwnerPath $ toSetRefOp <$> ownerMap'
            , Map.singleton [pathq|/owners|] $
                (const (Nothing, SoAbsent)) <$>
                  Map.mapKeys unNamespace (ownerMap `Map.difference` ownerMap'))
        toOwnerPath :: Namespace -> Path.Path
        toOwnerPath s = [pathq|/owners|] :/ unNamespace s
        toSetRefOp ns = ConstChange Nothing [
          someWireable $ Path.toText Path.unName $
          Root :/ [nameq|clients|] :/ ns]
        viewAs i dd =
          let
            theirSeg = pathNameFor i
            theirTime = unTimeDelta $ Map.findWithDefault
              (error "Can't rewrite message for unconnected client") i
              timingMap
            alterTime (ConstChange att [SomeWireValue (WireValue WtFloat t)]) =
              ConstChange att $ pure $ someWireable $ subtract theirTime t
            alterTime _ = error "Weird data back out of VS"
            fiddleDataChanges p dc
              | p `Path.isChildOf` [pathq|/clients|] = alterTime dc
              | p == [pathq|/self|] = toSetRefOp theirName
              | otherwise = dc
          in
            AL.fmapWithKey fiddleDataChanges dd
        -- This function trusts that the valuespace has completely validated the
        -- actions the client can perform (i.e. can only change the display name
        -- of a client)
        handleApiRequest
          :: Monad m
          => FrDigest 'Provider 'Update
          -> Protocol a (ClientEvent i SomeTrDigest) b' b m ()
        handleApiRequest frpd =
            sendFwd $ ClientData selfAddr $ SomeTrDigest $
            (trpdEmpty $ frpdNs frpd) {trpdData = frpdData frpd}

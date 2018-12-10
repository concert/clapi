{-# LANGUAGE
    GADTs
  , OverloadedStrings
#-}

module Clapi.RelayApi (relayApiProto, PathSegable(..)) where

import Control.Monad (void)
import Control.Monad.Trans (lift)
import Data.Bifunctor (bimap)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import qualified Data.Text as Text

import Clapi.PerClientProto (ClientEvent(..), ServerEvent(..))
import Clapi.Types
  ( TrDigest(..), TrpDigest(..), FrDigest(..), FrpDigest(..), WireValue(..)
  , TimeStamped(..), Editable(..))
import Clapi.Types.AssocList (alSingleton, alFromMap, alFmapWithKey, alFromList)
import Clapi.Types.Base (InterpolationLimit(ILUninterpolated))
import Clapi.Types.Definitions (tupleDef, structDef, arrayDef)
import Clapi.Types.Digests
  (DefOp(OpDefine), DataChange(..), FrcUpdateDigest(..), DataDigest, ContOps)
import Clapi.Types.SequenceOps (SequenceOp(..))
import Clapi.Types.Path (Seg, pattern Root, pattern (:/), Namespace(..))
import qualified Clapi.Types.Path as Path
import Clapi.Types.Tree (TreeType(..), unbounded, ttString, ttFloat, ttRef)
import Clapi.Types.Wire
  (WireType(..), SomeWireValue(..), someWireable, someWv)
import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)
import Clapi.TH (pathq, segq)
import Clapi.TimeDelta (tdZero, getDelta, TimeDelta(..))

class PathSegable a where
    pathNameFor :: a -> Seg

dn :: Seg
dn = [segq|display_name|]

relayApiProto ::
    forall i. (Ord i, PathSegable i) =>
    i ->
    Protocol
        (ClientEvent i (TimeStamped TrDigest))
        (ClientEvent i TrDigest)
        (ServerEvent i FrDigest)
        (Either (Map Namespace i) (ServerEvent i FrDigest))
        IO ()
relayApiProto selfAddr =
    publishRelayApi >> steadyState mempty mempty
  where
    publishRelayApi = sendFwd $ ClientData selfAddr $ Trpd $ TrpDigest
      rns
      mempty
      (Map.fromList $ bimap Tagged OpDefine <$>
        [ ([segq|build|], tupleDef "builddoc"
             (alSingleton [segq|commit_hash|] $ ttString "banana")
             ILUninterpolated)
        , (clock_diff, tupleDef
             "The difference between two clocks, in seconds"
             (alSingleton [segq|seconds|] $ ttFloat unbounded)
             ILUninterpolated)
        , (dn, tupleDef
             "A human-readable name for a struct or array element"
             (alSingleton [segq|name|] $ ttString "")
             ILUninterpolated)
        , ([segq|client_info|], structDef
             "Info about a single connected client" $ staticAl
             [ (dn, (Tagged dn, Editable))
             , (clock_diff, (Tagged clock_diff, ReadOnly))
             ])
        , ([segq|clients|], arrayDef "Info about the connected clients"
             Nothing (Tagged [segq|client_info|]) ReadOnly)
        , ([segq|owner_info|], tupleDef "owner info"
             (alSingleton [segq|owner|]
               -- FIXME: want to make Ref's Seg tagged...
               $ ttRef [segq|client_info|])
             ILUninterpolated)
        , ([segq|owners|], arrayDef "ownersdoc"
             Nothing (Tagged [segq|owner_info|]) ReadOnly)
        , ([segq|self|], tupleDef "Which client you are"
             (alSingleton [segq|info|] $ ttRef [segq|client_info|])
             ILUninterpolated)
        , ([segq|relay|], structDef "topdoc" $ staticAl
          [ ([segq|build|], (Tagged [segq|build|], ReadOnly))
          , ([segq|clients|], (Tagged [segq|clients|], ReadOnly))
          , ([segq|owners|], (Tagged [segq|owners|], ReadOnly))
          , ([segq|self|], (Tagged [segq|self|], ReadOnly))])
        ])
      (alFromList
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
    rns = Namespace [segq|relay|]
    clock_diff = [segq|clock_diff|]
    selfSeg = pathNameFor selfAddr
    selfClientPath = Root :/ [segq|clients|] :/ selfSeg
    staticAl = alFromMap . Map.fromList
    steadyState
      :: Map i TimeDelta -> Map Namespace Seg -> Protocol
            (ClientEvent i (TimeStamped TrDigest))
            (ClientEvent i TrDigest)
            (ServerEvent i FrDigest)
            (Either (Map Namespace i) (ServerEvent i FrDigest))
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
              pubUpdate (alFromList
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
            pubUpdate (alSingleton ([pathq|/clients|] :/ cSeg :/ clock_diff)
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
        pubUpdate dd co = sendFwd $ ClientData selfAddr $ Trpd $ TrpDigest
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
            ServerData cAddr d ->
              case d of
                Frcud frcud ->
                  sendRev $ ServerData cAddr $ Frcud
                  $ frcud {frcudData = viewAs cAddr $ frcudData frcud}
                x@(Frcrd _) -> void $ sequence $ Map.mapWithKey
                  (\addr _ -> sendRev $ ServerData addr x) timingMap
                Frpd frpd -> if frpdNamespace frpd == rns
                  then handleApiRequest frpd
                  else sendRev se
                _ -> sendRev se
            _ -> sendRev se
          steadyState timingMap ownerMap
        ownerChangeInfo :: Map Namespace Seg -> (DataDigest, ContOps args)
        ownerChangeInfo ownerMap' =
            ( alFromMap $ Map.mapKeys toOwnerPath $ toSetRefOp <$> ownerMap'
            , Map.singleton [pathq|/owners|] $
                (const (Nothing, SoAbsent)) <$>
                  Map.mapKeys unNamespace (ownerMap `Map.difference` ownerMap'))
        toOwnerPath :: Namespace -> Path.Path
        toOwnerPath s = [pathq|/owners|] :/ unNamespace s
        toSetRefOp ns = ConstChange Nothing [
          someWireable $ Path.toText Path.unSeg $
          Root :/ [segq|clients|] :/ ns]
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
              | p `Path.isChildOf` [pathq|/relay/clients|] = alterTime dc
              | p == [pathq|/relay/self|] = toSetRefOp theirSeg
              | otherwise = dc
          in
            alFmapWithKey fiddleDataChanges dd
        -- This function trusts that the valuespace has completely validated the
        -- actions the client can perform (i.e. can only change the display name
        -- of a client)
        handleApiRequest frpd =
            sendFwd $ ClientData selfAddr $ Trpd $ TrpDigest
              (frpdNamespace frpd) mempty mempty (frpdData frpd) mempty mempty

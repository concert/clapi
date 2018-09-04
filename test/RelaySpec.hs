{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE
    FlexibleInstances
  , LambdaCase
  , OverloadedStrings
  , PartialTypeSignatures
  , TypeFamilies
  , TypeFamilyDependencies
#-}

module RelaySpec where

import Test.Hspec

import Prelude hiding (pred)
import Control.Monad (unless, forM_, (>=>), forever)
import Control.Monad.Trans (lift)
import Data.Either (isRight)
import Data.Int (Int32)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.MultiSet as MS
import Data.Proxy
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Printf

import qualified Data.Map.Mos as Mos

import Clapi.TH
import Clapi.Protocol
  (waitThen, waitThenRevOnly, sendFwd, sendRev, runEffect, (<<->), Protocol)
import Clapi.PerClientProto (ClientEvent(..), ServerEvent(..))
import Clapi.Relay (relay, RelayState(..))
import Clapi.Types.AssocList
  ( alSingleton, unsafeMkAssocList, alInsert, alLookup)
import Clapi.Types.Base (InterpolationLimit(..))
import Clapi.Types.Definitions
  ( ArrayDefinition(..), StructDefinition(..), TupleDefinition(..)
  , arrayDef, structDef, tupleDef
  , Editable(..), Definition(..), PostDefinition(..))
import Clapi.Types.Digests
import Clapi.Types.SequenceOps (SequenceOp(..))
import Clapi.Types.Messages (DataErrorIndex(..), SubErrorIndex(..))
import Clapi.Types.Path (pattern Root, pattern (:/), Namespace(..), Seg, Path)
import Clapi.Types.Tree (TreeType(..), unbounded)
import Clapi.Types.Wire (WireValue(..))

import Clapi.Internal.Valuespace (DefMap)
import Instances ()


spec :: Spec
spec = do
    it "informs clients of root children" $ testRelay mempty $ do
      -- On connect:
      sendFwd $ ClientConnect "Observer" "observer"
      expect [emptyRootDig "observer"]
      sendFwd $ ClientConnect "Owner" "owner"
      expect [emptyRootDig "owner"]

      -- Everyone on claim:
      sendFwd $ ClientData "owner" $ Trpd $ simpleClaim foo
      expectSet $ nsExists fooNs <$> ["observer", "owner"]

      -- Newcomer of existing on connect:
      sendFwd $ ClientConnect "Newbie" "newcomer"
      expect [nsExists fooNs "newcomer"]

      -- Everyone on relinquish:
      sendFwd $ ClientDisconnect "owner"
      expectSet $ nsCease fooNs <$> ["observer", "newcomer"]

    it "rejects empty claims" $ testRelay mempty $ do
      sendFwd $ ClientConnect "Owner" "owner"
      expect [emptyRootDig "owner"]
      sendFwd $ ClientData "owner" $ Trpd $ trpdEmpty fooNs
      expectFrped "owner" [(NamespaceError fooNs, "Empty namespace claim")]

    it "does not reject empty owner updates" $ testRelay mempty $ do
      sendFwd $ ClientConnect "Owner" "owner"
      sendFwd $ ClientData "owner" $ Trpd $ simpleClaim foo
      expect [emptyRootDig "owner", nsExists fooNs "owner"]
      sendFwd $ ClientData "owner" $ Trpd $ trpdEmpty fooNs

    it "rejects ownership claims on owned namespace" $ testRelay mempty $ do
      sendFwd $ ClientConnect "Owner" "owner"
      sendFwd $ ClientData "owner" $ Trpd $ simpleClaim foo
      expect [emptyRootDig "owner", nsExists fooNs "owner"]

      sendFwd $ ClientConnect "Usurper" "userper"
      expect [nsExists fooNs "userper"]
      sendFwd $ ClientData "userper" $ Trpd $ simpleClaim foo
      expectFrped "userper" [(NamespaceError fooNs, "Already owned")]

    it "allows a new owner to claim a relinquished namespace" $
      testRelay mempty $ do
        sendFwd $ ClientConnect "Owner1" "owner1"
        sendFwd $ ClientConnect "Owner2" "owner2"
        expect $ emptyRootDig <$> ["owner1", "owner2"]

        sendFwd $ ClientData "owner1" $ Trpd $ simpleClaim foo
        expectSet $ nsExists fooNs <$> ["owner1", "owner2"]
        sendFwd $ ClientData "owner1" $ Trprd $ TrprDigest fooNs
        expectSet $ nsCease fooNs <$> ["owner1", "owner2"]

        sendFwd $ ClientData "owner2" $ Trpd $ simpleClaim foo
        expectSet $ nsExists fooNs <$> ["owner1", "owner2"]

    it "rejects owner (un)subscriptions" $
      let
        pt op n = trcsdEmpty { trcsdPostTypes = Map.singleton (fooNs, n) op }
        t op n = trcsdEmpty { trcsdTypes = Map.singleton (fooNs, n) op }
        d op p = trcsdEmpty { trcsdData = Map.singleton (fooNs, p) op }
      in
        forM_ [OpSubscribe, OpUnsubscribe] $ \op ->
          -- "foo"s exists, "bar"s do not, but existance should be irrelevent to
          -- the error...
          forM_ [ pt op $ Tagged @PostDefinition foo
                , pt op $ Tagged @PostDefinition bar
                , t op $ Tagged @Definition foo
                , t op $ Tagged @Definition bar
                , d op $ Root
                , d op $ Root :/ bar
                ] $ \subDig ->
            testRelay mempty $ do
              sendFwd $ ClientConnect "Owner" "owner"
              sendFwd $ ClientData "owner" $ Trpd $ simpleClaim foo
              expect [emptyRootDig "owner", nsExists fooNs "owner"]
              sendFwd $ ClientData "owner" $ Trpd $
                postDefine foo (postDef' "fooy" [(x, TtInt64 unbounded)]) $
                trpdEmpty fooNs
              sendFwd $ ClientData "owner" $ Trcsd subDig
              expect [ err "owner" (NamespaceError fooNs)
                       "Acted as client on own namespace"
                     , ServerDisconnect "owner"
                     ]

    it "rejects subscriptions to non-existent entities" $ testRelay mempty $
      -- FIXME: perhaps subscriptions to non-existent entities should just
      -- result in unsubscription notices, just like the resource going away,
      -- because it could have gone away between the client trying to subscribe
      -- and the message arriving at the relay?
      let
        badSub
          :: Subscribe entity => EntityId entity -> SubErrorIndex -> Text
          -> TestProtocol ()
        badSub id_ ei msg = do
          subscribe fooNs id_ "sub"
          expect [ServerData "sub" $ Frcsd $
            mempty {frcsdErrors = Map.singleton ei [msg]}]
        pd = postDef' "fooy" [(x, TtInt64 unbounded)]
        bar1 = [segq|bar1|]
      in do
        sendFwd $ ClientConnect "Subscriber" "sub"
        expect [emptyRootDig "sub"]

        -- Invalid NS:
        badSub root (NamespaceSubError fooNs) "Namespace not found"

        sendFwd $ ClientConnect "Owner" "owner"
        expect [emptyRootDig "owner"]
        sendFwd $ ClientData "owner" $ Trpd $
          -- NB: this set of definitions exercises the "empty array problem"
          postDefine foo pd $
          define foo (arrayDef' "Array of single-int tuples" bar Editable) $
          define bar intDef $
          trpdEmpty fooNs
        expectSet $ nsExists fooNs <$> ["owner", "sub"]

        badSub (root :/ bar1) (PathSubError fooNs $ root :/ bar1) "Path not found"

        sendFwd $ ClientData "owner" $ Trpd $
          ownerSet (root :/ bar1) [WireValue @Int32 1] $
          trpdEmpty fooNs

        -- We check that now the thing we just failed to subscribe to _is_
        -- defined we don't suddenly start getting data:
        verifyNoDataSub "owner" "sub" fooNs (root :/ bar1) [WireValue @Int32 16]

        -- Invalid PostDefinition and Definition IDs:
        badSub bazPdn (PostTypeSubError fooNs bazPdn) "Missing post def"
        badSub bazTn (TypeSubError fooNs bazTn) "Missing def"

        -- And again check that once the owner does define these, we haven't got
        -- registered subscriptions already:
        sendFwd $ ClientData "owner" $ Trpd $
          define baz strDef $
          postDefine baz pd $
          trpdEmpty fooNs
        verifyNoPdSub "owner" "sub" fooNs bazPdn
        verifyNoTySub "owner" "sub" fooNs bazTn

    it "handles legitimate subscriptions" $ testRelay mempty $ do
      -- Make an API with an owner:
      sendFwd $ ClientConnect "Owner" "owner"
      sendFwd $ ClientData "owner" $ Trpd $ simpleClaim foo
      expect [emptyRootDig "owner", nsExists fooNs "owner"]

      -- Connect a subscriber:
      sendFwd $ ClientConnect "Subscriber" "sub"
      expect [nsExists fooNs "sub"]

      -- Subscribe to some data and check data and type subscriptions:
      withSubscription fooNs root "sub" $ do
        expect [ ServerData "sub" $ Frcud $ (frcudEmpty fooNs)
          { frcudDefinitions = Map.singleton fooTn $ OpDefine intDef
          , frcudTypeAssignments = Map.singleton Root (fooTn, Editable)
          , frcudData = alSingleton Root $ ConstChange Nothing [WireValue @Int32 12]
          }]
        verifyDataSub "owner" "sub" fooNs Root [WireValue @Int32 13]
        verifyTySub "owner" "sub" fooNs fooTn

      -- After unsubscribing from the data, check that
      -- a) The data subscription is terminated
      verifyNoDataSub "owner" "sub" fooNs Root [WireValue @Int32 14]
      -- b) The type subscription remains
      verifyTySub "owner" "sub" fooNs fooTn

      -- Ditch the residual type subscription
      unsubscribe fooNs fooTn "sub"
      verifyNoTySub "owner" "sub" fooNs fooTn

      -- Test regulare sub/unsub cycle for Definitions
      withSubscription fooNs fooTn "sub" $ do
        expect [ ServerData "sub" $ Frcud $ (frcudEmpty fooNs)
          { frcudDefinitions = Map.singleton fooTn $ OpDefine intDef }]
        verifyTySub "owner" "sub" fooNs fooTn
      verifyNoTySub "owner" "sub" fooNs fooTn

      -- Test sub/unsub cycle for PostDefinitions too
      let pd = postDef' "fooy" [(x, TtInt64 unbounded)]
      sendFwd $ ClientData "owner" $ Trpd $ postDefine foo pd $ trpdEmpty fooNs
      withSubscription fooNs fooPdn "sub" $ do
        expect [ ServerData "sub" $ Frcud $ (frcudEmpty fooNs)
         { frcudPostDefs = Map.singleton fooPdn $ OpDefine pd }]
        verifyPdSub "owner" "sub" fooNs fooPdn
      verifyNoPdSub "owner" "sub" fooNs fooPdn


    it "should not send empty updates on empty subscriptions" $
      testRelay mempty $ do
        sendFwd $ ClientConnect "Subscriber" "sub"
        expect [emptyRootDig "sub"]

        sendFwd $ ClientData "sub" $ Trcsd $ mempty
        nothingWaiting


    it "is permissive/idempotent on client unsubscription" $ testRelay mempty $
      -- The unsubscribed entity may have already gone away and the relay's
      -- notifications crossed with the client's on the wire. The only case
      -- where we are not permissive is if the particular client owns the entity
      -- it's trying to unsubscribe from!
      let
        checkAlreadyUnsub
          :: Subscribe entity => EntityId entity -> TestProtocol ()
        checkAlreadyUnsub id_ =
          _sendSub OpUnsubscribe fooNs id_ "sub" >> nothingWaiting
      in do
        sendFwd $ ClientConnect "Subscriber" "sub"
        expect [emptyRootDig "sub"]
        -- Ns doesn't exist:
        checkAlreadyUnsub root

        sendFwd $ ClientConnect "Owner" "owner"
        expect [emptyRootDig "owner"]
        let pd = postDef' "fooy" [(x, TtInt64 unbounded)]
        sendFwd $ ClientData "owner" $ Trpd $
          postDefine foo pd $ simpleClaim foo
        expectSet $ nsExists fooNs <$> ["owner", "sub"]

        -- PostDefinition, Definition and data don't exist:
        checkAlreadyUnsub $ Tagged @PostDefinition bar
        checkAlreadyUnsub $ Tagged @Definition bar
        checkAlreadyUnsub $ Root :/ bar

        -- PostDefinition, Definition and data exist, but never subscribed
        checkAlreadyUnsub fooPdn
        checkAlreadyUnsub fooTn
        checkAlreadyUnsub root

        -- PostDefinition, Definition and data exist, previous subscription
        withSubscription fooNs fooPdn "sub" $ do
          expect [ServerData "sub" $ Frcud $ (frcudEmpty fooNs)
            { frcudPostDefs = Map.singleton fooPdn $ OpDefine pd }]
        checkAlreadyUnsub fooPdn
        verifyNoPdSub "owner" "sub" fooNs fooPdn

        withSubscription fooNs fooTn "sub" $ do
          expect [ServerData "sub" $ Frcud $ (frcudEmpty fooNs)
            { frcudDefinitions = Map.singleton fooTn $ OpDefine intDef }]
        checkAlreadyUnsub fooTn
        verifyNoTySub "owner" "sub" fooNs fooTn

        withSubscription fooNs root "sub" $ do
          expect [ ServerData "sub" $ Frcud $ (frcudEmpty fooNs)
            { frcudDefinitions = Map.singleton fooTn $ OpDefine intDef
            , frcudTypeAssignments = Map.singleton Root (fooTn, Editable)
            , frcudData = alSingleton Root $
              ConstChange Nothing [WireValue @Int32 12]
            }]
        checkAlreadyUnsub root
        verifyNoDataSub "owner" "sub" fooNs root [WireValue @Int32 15]


    let sub_owner_preamble = do
          sendFwd $ ClientConnect "Owner" "owner"
          sendFwd $ ClientConnect "Subscriber" "sub"
          expect $ emptyRootDig <$> ["owner", "sub"]

          sendFwd $ ClientData "owner" $ Trpd $ structClaim foo
          expectSet $ nsExists fooNs <$> ["owner", "sub"]

    it "unsubscribes clients on client disconnect" $ testRelay mempty $ do
      -- i.e. after client disconnects, the relay never tries to send another
      -- message to that client.
      sub_owner_preamble
      subscribe fooNs fooTn "sub"
      expect [ ServerData "sub" $ Frcud $ (frcudEmpty fooNs)
        { frcudDefinitions = Map.singleton fooTn $ OpDefine $
          structDef' "test" [(foo, [segq|texty|]), (bar, [segq|inty|])]
        }]

      sendFwd $ ClientDisconnect "sub"
      verifyNoTySub "owner" "sub" fooNs fooTn
      sendFwd $ ClientDisconnect "owner"

    it "unsubscribes clients on relay disconnect" $ testRelay mempty $ do
      -- i.e. after the relay disconnects a client, the relay never tries to
      -- send another message to that client.
      sub_owner_preamble
      subscribe fooNs fooTn "sub"
      expect [ ServerData "sub" $ Frcud $ (frcudEmpty fooNs)
        { frcudDefinitions = Map.singleton fooTn $ OpDefine $
          structDef' "test" [(foo, [segq|texty|]), (bar, [segq|inty|])]
        }]

      -- We have to trigger a client disconnect by being a bad provider:
      sendFwd $ ClientData "sub" $ Trpd $ simpleClaim foo
      expectFrped "sub" [(NamespaceError fooNs, "Already owned")]

      verifyNoTySub "owner" "sub" fooNs fooTn

    it "unsubscribes clients on owner disconnect and disowns" $
      -- Also that the client gets notified of the deletion of the owner's data.
      testRelay mempty $ let textyTn = Tagged @Definition [segq|texty|] in do
        sub_owner_preamble
        subscribe fooNs textyTn "sub"
        expect [ ServerData "sub" $ Frcud $ (frcudEmpty fooNs)
          { frcudDefinitions = Map.singleton textyTn $ OpDefine strDef }]

        sendFwd $ ClientDisconnect "owner"
        expectSet
          [ ServerData "sub" $ Frcsd $
            frcsdEmpty {frcsdTypeUnsubs = Mos.singleton fooNs textyTn}
          , nsCease fooNs "sub"
          ]

    it "doesn't forward empty subscriber bundles to owner" $
      testRelay mempty $ do
        sub_owner_preamble
        sendFwd $ ClientData "sub" $ Trcud $ trcudEmpty fooNs

    it "validates subscriber mutations and forwards valid" $
      testRelay mempty $ do
        sub_owner_preamble
        sendFwd $ ClientData "sub" $ Trcud $
          subSet (Root :/ foo) [WireValue @Text "hello"] $
          subSet (Root :/ bar) [WireValue @Int32 3, WireValue @Int32 4] $
          trcudEmpty fooNs
        expectSubErr "sub" (PathError $ Root :/ bar) "Mismatched numbers"
        expect [ServerData "owner" $ Frpd $ (frpdEmpty fooNs)
          { frpdData = alSingleton (Root :/ foo) $
            ConstChange Nothing [WireValue @Text "hello"]
          }]

    it "validates owner claims" $ testRelay mempty $ do
      sendFwd $ ClientConnect "Owner" "owner"
      expect [emptyRootDig "owner"]

      sendFwd $ ClientData "owner" $ Trpd $
        ownerSet root [WireValue @Text "Not an int"] $
        define foo intDef $
        trpdEmpty fooNs
      expectFrped "owner" [(PathError Root, "Type mismatch")]

    it "validates owner mutations" $ testRelay mempty $ do
      sendFwd $ ClientConnect "Owner" "owner"
      sendFwd $ ClientData "owner" $ Trpd $ simpleClaim foo
      expect [emptyRootDig "owner", nsExists fooNs "owner"]

      sendFwd $ ClientData "owner" $ Trpd $
        ownerSet root [WireValue @Text "Not an int"] $ trpdEmpty fooNs
      expectFrped "owner" [(PathError Root, "Type mismatch")]

    it "rejects orphan data" $ testRelay mempty $ do
      sendFwd $ ClientConnect "Owner" "owner"
      expect [emptyRootDig "owner"]
      sendFwd $ ClientData "owner" $ Trpd $
        ownerSet (Root :/ baz) [WireValue @Text "Orphan"] $ structClaim foo
      expectFrped "owner" [(PathError Root, "ExtraChild baz")]

  where
    rootDig client = ServerData client . Frcrd . FrcRootDigest
    emptyRootDig client = rootDig client mempty
    nsExists ns client = rootDig client $ Map.singleton ns $ SoAfter Nothing
    nsCease ns client = rootDig client $ Map.singleton ns $ SoAbsent
    err client i msg = errs client [(i, [msg])]
    errs client = ServerData client . Frped . FrpErrorDigest . Map.fromList

    verifySub
      :: Subscribe entity
      => String -> String -> Namespace -> EntityId entity -> Mutator entity
      -> TestProtocol ()
    verifySub ownerId subId ns ident mutation = do
      existing <- retrieve ns ident
      let new = mutate mutation existing
      sendFwd $ ClientData ownerId $ Trpd $ mkTrpd ns ident new
      expect [ServerData subId $ Frcud $ mkFrcud ns ident new]
      sendFwd $ ClientData ownerId $ Trpd $ mkTrpd ns ident existing
      expect [ServerData subId $ Frcud $ mkFrcud ns ident existing]

    verifyDataSub = verifySub @[WireValue]
    verifyTySub o s n i = verifySub @Definition o s n i "changed doc"
    verifyPdSub o s n i = verifySub @PostDefinition o s n i "changed doc"

    -- FIXME: this will only work if there are _no subscribers at all_ to the
    -- entity
    verifyNoSub
      :: Subscribe entity
      => String -> String -> Namespace -> EntityId entity -> Mutator entity
      -> TestProtocol ()
    verifyNoSub ownerId subId ns ident mutation = do
      existing <- retrieve ns ident
      let new = mutate mutation existing
      sendFwd $ ClientData ownerId $ Trpd $ mkTrpd ns ident new
      nothingWaiting
      sendFwd $ ClientData ownerId $ Trpd $ mkTrpd ns ident existing
      nothingWaiting

    verifyNoDataSub = verifyNoSub @[WireValue]
    verifyNoTySub o s n i = verifyNoSub @Definition o s n i "changed doc"
    verifyNoPdSub o s n i = verifyNoSub @PostDefinition o s n i "changed doc"

    intDef = tupleDef'
      "A single unbounded integer" [(x, TtInt32 unbounded)] ILUninterpolated
    strDef = tupleDef' "Any string" [(x, TtString "")] ILUninterpolated

    simpleClaim name =
      ownerSet Root [WireValue @Int32 12] $
      define name intDef $
      trpdEmpty $ Namespace name

    structClaim name =
      ownerSet (Root :/ bar) [WireValue @Int32 2] $
      ownerSet (Root :/ foo) [WireValue @Text "one"] $
      define name (structDef' "test"
        [ (foo, [segq|texty|])
        , (bar, [segq|inty|])
        ]) $
      define [segq|inty|] intDef $
      define [segq|texty|] strDef $
      trpdEmpty $ Namespace name

all' :: [a -> Bool] -> a -> Bool
all' preds a = all id $ preds <*> pure a

init' :: [a] -> [a]
init' [] = []
init' as = init as

showProtocol :: (Show a, Show b) => Protocol a a b b IO ()
showProtocol = forever $ waitThen
  (\a -> lift (putStrLn $ printf "-> %s" $ show a) >> sendFwd a)
  (\b -> lift (putStrLn $ printf "<- %s" $ show b) >> sendRev b)

type TestProtocol r = Protocol
  Void (ClientEvent String TrDigest)
  Void (Either (Map Namespace String) (ServerEvent String FrDigest))
  IO r

testRelay :: RelayState String -> TestProtocol () -> IO ()
testRelay state prot = runEffect $ (prot >> nothingWaiting) <<-> relay state

waitUntil :: Monad m => (a -> Bool) -> Protocol Void x1 x2 a m a
waitUntil pred = do
  a <- waitThenRevOnly return
  if pred a then return a else waitUntil pred

collateUntil
  :: Monad m => (a -> Bool) -> (a -> Bool) -> Protocol Void x1 x2 a m [a]
collateUntil counts pred = reverse <$> go []
  where
    go l = do
      a <- waitUntil counts
      if pred a then return $ a : l else go $ a : l

expectSet_ :: (Ord a, Show a) => (a -> Bool) -> [a] -> Protocol Void x1 x2 a IO ()
expectSet_ counts expected = go $ MS.fromList expected
  where
    go remaining = unless (null remaining) $ do
      a <- waitUntil counts
      if a `MS.member` remaining
        then go $ MS.delete a remaining
        else lift $ expectationFailure $ printf
          "Unexpected message:\n%s\nExpected one of:\n%s"
          (show a) (intercalate "\n" $ show <$> expected)

-- | Fail if the given list of messages is not sent out by the relay, but don't
--   worry about the precise order. Must account for _all_ the messages you
--   expect though.
expectSet :: _ => [a] -> Protocol Void x1 x2 (Either x3 a) IO ()
expectSet = expectSet_ isRight . fmap Right

-- | Fail if the given list of messages is not sent out by the relay in the
--   given order.
expect :: _ => [a] -> Protocol Void x1 x2 (Either x3 a) IO ()
expect = mapM_ nextShouldBe

expectFrped
  :: _ => String -> [(DataErrorIndex, String)]
  -> Protocol Void x1 x2 (Either x3 (ServerEvent String FrDigest)) IO ()
expectFrped clientId expected = do
    onNext isRight handleFrped
    expect [ServerDisconnect clientId]
  where
    handleFrped (Right (ServerData i (Frped frped))) = do
      i `shouldBe` clientId
      forM_ expected $ \(ei, expMsg) ->
        case Map.lookup ei $ frpedErrors frped of
          Nothing -> expectationFailure $
            printf "%s not in errors:\n%s" (show ei) (show $ frpedErrors frped)
          Just msgs -> mapM_ (`shouldContain` expMsg) $ Text.unpack <$> msgs
    handleFrped _ = expectationFailure "Not an error digest"

-- FIXME: might this be better is we had explicit error digests for the client,
-- rather than intermingling them in the data updates?
-- | Expect a subscriber to have been given an error
expectSubErr
  :: String -> DataErrorIndex -> String
  -> Protocol Void v1 v2 (Either x3 (ServerEvent String FrDigest)) IO ()
expectSubErr clientId ei expMsg = onNext isRight handleFrcud
  where
    handleFrcud (Right (ServerData i (Frcud frcud))) = do
      i `shouldBe` clientId
      case Map.lookup ei $ frcudErrors frcud of
        Nothing -> expectationFailure $
          printf "%s not in errors:\n%s" (show ei) (show $ frcudErrors frcud)
        Just msgs -> mapM_ (`shouldContain` expMsg) $ Text.unpack <$> msgs
    handleFrcud _ = expectationFailure "Not a client data update"


onNext :: Monad m => (a -> Bool) -> (a -> m r) -> Protocol Void x1 x2 a m r
onNext counts action = waitUntil counts >>= lift . action

nextShouldBe :: _ => a -> Protocol Void x1 x2 (Either x3 a) IO ()
nextShouldBe expected = onNext isRight (`shouldBe` Right expected)

nextShouldSatisfy
  :: Show a => (a -> Bool) -> Protocol Void x1 x2 (Either x3 a) IO a
nextShouldSatisfy pred = onNext isRight $
  either (error "impossible Left") (\a -> (a `shouldSatisfy` pred) >> return a)

-- | Connect a named witness client and perform the given protocol action with
--   them connected. NB: we check that we have no outstanding messages before
--   handing over control.
withWitness :: String -> TestProtocol r -> TestProtocol r
withWitness ident proto = do
    sendFwd $ ClientConnect "Witness" ident
    unexpected <- init' <$> collateUntil isRight witnessConnected
    unless (null unexpected) $ lift $ expectationFailure $ printf
      "%d unexpected messages:\n%s\nExpected no messages."
      (length unexpected)
      (intercalate "\n" $ show <$> unexpected)
    a <- proto
    sendFwd $ ClientDisconnect ident
    return a
  where
    witnessConnected = \case
      Right (ServerData ident' (Frcrd (FrcRootDigest _))) -> ident' == ident
      _ -> False

-- | Fail if there is are unahandled messages from the relay waiting for us.
nothingWaiting :: TestProtocol ()
nothingWaiting = withWitness "nothing waiting" $ return ()

unOpDefine :: DefOp a -> Maybe a
unOpDefine = \case
  OpDefine a -> Just a
  _ -> Nothing

unConstChange :: DataChange -> Maybe [WireValue]
unConstChange = \case
  ConstChange _ wvs -> Just wvs
  _ -> Nothing

class Subscribe entity where
  type EntityId entity = ident | ident -> entity
  mkTrcsd :: Namespace -> EntityId entity -> SubOp -> TrcSubDigest
  mkFrcsd :: Namespace -> EntityId entity -> FrcSubDigest
  mkTrpd :: Namespace -> EntityId entity -> entity -> TrpDigest
  mkFrcud :: Namespace -> EntityId entity -> entity -> FrcUpdateDigest
  _repr :: proxy entity -> String
  extractEntity :: EntityId entity -> FrcUpdateDigest -> Maybe entity
  type Mutator entity :: *
  mutate :: Mutator entity -> entity -> entity

  _sendSub :: SubOp -> Namespace -> EntityId entity -> String -> TestProtocol ()
  _sendSub op ns name clientId = sendFwd $ ClientData clientId $ Trcsd $
    mkTrcsd ns name op

instance Subscribe PostDefinition where
  type EntityId PostDefinition = Tagged PostDefinition Seg
  mkTrcsd ns name op = mempty {trcsdPostTypes = Map.singleton (ns, name) op}
  mkFrcsd ns name = mempty {frcsdPostTypeUnsubs = Mos.singleton ns name}
  mkTrpd ns name ent = (trpdEmpty ns)
    { trpdPostDefs = Map.singleton name $ OpDefine ent }
  mkFrcud ns name ent = (frcudEmpty ns)
    { frcudPostDefs = Map.singleton name $ OpDefine ent }
  _repr _ = "post def"
  extractEntity name = Map.lookup name . frcudPostDefs >=> unOpDefine
  type Mutator PostDefinition = Text
  mutate doc pd = pd { postDefDoc = doc }

instance Subscribe Definition where
  type EntityId Definition = Tagged Definition Seg
  mkTrcsd ns name op = mempty {trcsdTypes = Map.singleton (ns, name) op}
  mkFrcsd ns name = mempty {frcsdTypeUnsubs = Mos.singleton ns name}
  mkTrpd ns name ent = (trpdEmpty ns)
    { trpdDefinitions = Map.singleton name $ OpDefine ent }
  mkFrcud ns name ent = (frcudEmpty ns)
    { frcudDefinitions = Map.singleton name $ OpDefine ent }
  _repr _ = "def"
  extractEntity name = Map.lookup name . frcudDefinitions >=> unOpDefine
  type Mutator Definition = Text
  mutate doc = \case
    ArrayDef ad -> ArrayDef ad {arrDefDoc = doc}
    StructDef ad -> StructDef ad {strDefDoc = doc}
    TupleDef ad -> TupleDef ad {tupDefDoc = doc}

instance Subscribe [WireValue] where
  type EntityId [WireValue] = Path
  mkTrcsd ns path op = mempty {trcsdData = Map.singleton (ns, path) op}
  mkFrcsd ns path = mempty {frcsdDataUnsubs = Mos.singleton ns path}
  mkTrpd ns name ent = (trpdEmpty ns)
    { trpdData = alSingleton name $ ConstChange Nothing ent }
  mkFrcud ns name ent = (frcudEmpty ns)
    {frcudData = alSingleton name $ ConstChange Nothing ent }
  _repr _ = "data"
  extractEntity name = alLookup name . frcudData >=> unConstChange
  type Mutator [WireValue] = [WireValue]
  mutate wvs _ = wvs

subscribe
  :: Subscribe entity
  => Namespace -> EntityId entity -> String -> TestProtocol ()
subscribe = _sendSub OpSubscribe

unsubscribe
  :: Subscribe entity
  => Namespace -> EntityId entity -> String -> TestProtocol ()
unsubscribe ns name clientId = do
  _sendSub OpUnsubscribe ns name clientId
  expect [ServerData clientId $ Frcsd $ mkFrcsd ns name]

withSubscription
  :: Subscribe entity
  => Namespace -> EntityId entity -> String -> TestProtocol r -> TestProtocol r
withSubscription ns name clientId proto = do
  subscribe ns name clientId
  r <- proto
  unsubscribe ns name clientId
  return r

retrieve
  :: forall entity. Subscribe entity
  => Namespace -> EntityId entity -> TestProtocol entity
retrieve ns name =
  let
    etName = _repr $ Proxy @entity
    wname = "getting " ++ etName
  in withWitness wname $ do
    subscribe ns name wname
    Right (ServerData _ (Frcud frcud)) <- waitUntil isRight
    return $ fromMaybe (error $ "missing existing " ++ etName) $
        extractEntity name frcud

define :: Seg -> Definition -> TrpDigest -> TrpDigest
define name def trpd = trpd
  { trpdDefinitions = Map.insert (Tagged name) (OpDefine def) $
    trpdDefinitions trpd }

postDefine :: Seg -> PostDefinition -> TrpDigest -> TrpDigest
postDefine name def trpd = trpd
  { trpdPostDefs = Map.insert (Tagged name) (OpDefine def) $
    trpdPostDefs trpd }

ownerSet :: Path -> [WireValue] -> TrpDigest -> TrpDigest
ownerSet path values trpd = trpd
  { trpdData = alInsert path (ConstChange Nothing values) $ trpdData trpd }

subSet :: Path -> [WireValue] -> TrcUpdateDigest -> TrcUpdateDigest
subSet path values trcud = trcud
  { trcudData = alInsert path (ConstChange Nothing values) $ trcudData trcud }

foo, bar, baz, x:: Seg
foo = [segq|foo|]; bar = [segq|bar|]; baz = [segq|baz|]; x = [segq|x|]

fooNs, barNs, bazNs :: Namespace
fooNs = Namespace foo; barNs = Namespace bar; bazNs = Namespace baz

fooTn, barTn, bazTn :: Tagged Definition Seg
fooTn = Tagged foo; barTn = Tagged bar; bazTn = Tagged baz

fooPdn, barPdn, bazPdn :: Tagged PostDefinition Seg
fooPdn = Tagged foo; barPdn = Tagged bar; bazPdn = Tagged baz

-- | A non-polymorphic root path
root :: Path
root = Root

arrayDef' :: Text -> Seg -> Editable -> Definition
arrayDef' doc tn ed = arrayDef doc Nothing (Tagged tn) ed

structDef' :: Text -> [(Seg, Seg)] -> Definition
structDef' doc tys = structDef doc $ unsafeMkAssocList $
  fmap ((,Editable) . Tagged) <$> tys

tupleDef' :: Text -> [(Seg, TreeType)] -> InterpolationLimit -> Definition
tupleDef' doc tys il = tupleDef doc (unsafeMkAssocList tys) il

postDef' :: Text -> [(Seg, TreeType)] -> PostDefinition
postDef' doc tys = PostDefinition doc (unsafeMkAssocList $ fmap pure <$> tys)

defMap :: [(Seg, def)] -> DefMap def
defMap = Map.mapKeysMonotonic Tagged . Map.fromList

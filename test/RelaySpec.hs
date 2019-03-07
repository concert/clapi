{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
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
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.MultiSet as MS
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Printf

import qualified Data.Map.Mol as Mol
import qualified Data.Map.Mos as Mos

import Clapi.TH
import Clapi.Protocol
  (waitThen, waitThenRevOnly, sendFwd, sendRev, runEffect, (<<->), Protocol)
import Clapi.PerClientProto (ClientEvent(..), ServerEvent(..))
import Clapi.Relay (relay, RelayState(..))
import qualified Clapi.Types.AssocList as AL
import Clapi.Types.Base (InterpolationLimit)
import Clapi.Types.Definitions
  ( arrayDef, structDef, tupleDef
  , Editability(..), Definition(..), SomeDefinition(..), PostDefinition(..))
import Clapi.Types.Digests
import Clapi.Types.SequenceOps (SequenceOp(..))
import Clapi.Types.Name
  ( Name, castName
  , DataName, DefName, Namespace, PostArgName, PostDefName, TupMemberName
  )
import Clapi.Types.Path (pattern Root, pattern (:/), Path)
import Clapi.Types.Tree
  (SomeTreeType(..), unbounded, ttInt64, ttInt32, ttString)
import Clapi.Types.Wire (WireType(..), SomeWireValue(..), someWv)

import Clapi.Internal.Valuespace (DefMap, DefKey)
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
      sendFwd $ ClientData "owner" $ SomeTrDigest $ simpleClaim foo
      expectSet $ nsExists fooNs <$> ["observer", "owner"]

      -- Newcomer of existing on connect:
      sendFwd $ ClientConnect "Newbie" "newcomer"
      expect [nsExists fooNs "newcomer"]

      -- Everyone on relinquish:
      sendFwd $ ClientDisconnect "owner"
      expectSet $ nsCease fooNs <$> ["observer", "newcomer"]

    it "does not reject empty owner updates" $ testRelay mempty $ do
      sendFwd $ ClientConnect "Owner" "owner"
      sendFwd $ ClientData "owner" $ SomeTrDigest $ simpleClaim foo
      expect [emptyRootDig "owner", nsExists fooNs "owner"]
      sendFwd $ ClientData "owner" $ SomeTrDigest $ trpdEmpty fooNs

    it "rejects ownership claims on owned namespace" $ testRelay mempty $ do
      sendFwd $ ClientConnect "Owner" "owner"
      sendFwd $ ClientData "owner" $ SomeTrDigest $ simpleClaim foo
      expect [emptyRootDig "owner", nsExists fooNs "owner"]

      sendFwd $ ClientConnect "Usurper" "userper"
      expect [nsExists fooNs "userper"]
      sendFwd $ ClientData "userper" $ SomeTrDigest $ simpleClaim foo
      expectFrped "userper" [(NamespaceError fooNs, "Already owned")]

    it "allows a new owner to claim a relinquished namespace" $
      testRelay mempty $ do
        sendFwd $ ClientConnect "Owner1" "owner1"
        sendFwd $ ClientConnect "Owner2" "owner2"
        expect $ emptyRootDig <$> ["owner1", "owner2"]

        sendFwd $ ClientData "owner1" $ SomeTrDigest $ simpleClaim foo
        expectSet $ nsExists fooNs <$> ["owner1", "owner2"]
        sendFwd $ ClientData "owner1" $ SomeTrDigest $ Trprd fooNs
        expectSet $ nsCease fooNs <$> ["owner1", "owner2"]

        -- Check that the previously defined namespace is completely gone
        sendFwd $ ClientConnect "Subscriber" "sub"
        expect $ [emptyRootDig "sub"]
        subscribe fooNs fooDn "sub"
        expect $ [ServerData "sub" $ SomeFrDigest $ frcsdEmpty
          { frcsdTypeUnsubs = Mos.singleton fooNs fooDn
          }]
        sendFwd $ ClientDisconnect "sub"

        sendFwd $ ClientData "owner2" $ SomeTrDigest $ simpleClaim foo
        expectSet $ nsExists fooNs <$> ["owner1", "owner2"]

    it "rejects owner (un)subscriptions" $
      let
        pt op n = trcsdEmpty { trcsdPostTys = Map.singleton (fooNs, n) op }
        t op n = trcsdEmpty { trcsdTys = Map.singleton (fooNs, n) op }
        d op p = trcsdEmpty { trcsdData = Map.singleton (fooNs, p) op }
      in
        forM_ [OpSubscribe, OpUnsubscribe] $ \op ->
          -- "foo"s exists, "bar"s do not, but existance should be irrelevent to
          -- the error...
          forM_ [ pt op fooPdn
                , pt op barPdn
                , t op fooDn
                , t op barDn
                , d op $ Root
                , d op $ Root :/ bar
                ] $ \subDig ->
            testRelay mempty $ do
              sendFwd $ ClientConnect "Owner" "owner"
              sendFwd $ ClientData "owner" $ SomeTrDigest $ simpleClaim foo
              expect [emptyRootDig "owner", nsExists fooNs "owner"]
              sendFwd $ ClientData "owner" $ SomeTrDigest $
                postDefine foo (postDef' "fooy" [(x, ttInt64 unbounded)]) $
                trpdEmpty fooNs
              sendFwd $ ClientData "owner" $ SomeTrDigest subDig
              expect [ err "owner" (NamespaceError fooNs)
                       "Acted as client on own namespace"
                     , ServerDisconnect "owner"
                     ]

    it "rejects subscriptions to non-existent entities" $ testRelay mempty $
      let
        badSub :: (Subscribe entity) => EntityId entity -> TestProtocol ()
        badSub id_ = do
          subscribe fooNs id_ "sub"
          expect [ServerData "sub" $ SomeFrDigest $ mkFrcsd fooNs id_]
        pd = postDef' "fooy" [(x, ttInt64 unbounded)]
        bar1 = [n|bar1|]
      in do
        sendFwd $ ClientConnect "Subscriber" "sub"
        expect [emptyRootDig "sub"]

        -- Invalid NS:
        badSub root

        sendFwd $ ClientConnect "Owner" "owner"
        expect [emptyRootDig "owner"]
        sendFwd $ ClientData "owner" $ SomeTrDigest $
          -- NB: this set of definitions exercises the "empty array problem"
          postDefine foo pd $
          define foo (arrayDef' "Array of single-int tuples" bar Editable) $
          define bar intDef $
          trpdEmpty fooNs
        expectSet $ nsExists fooNs <$> ["owner", "sub"]

        badSub (root :/ bar1)

        sendFwd $ ClientData "owner" $ SomeTrDigest $
          ownerSet (root :/ bar1) [someWv WtInt32 1] $
          trpdEmpty fooNs

        -- We check that now the thing we just failed to subscribe to _is_
        -- defined we don't suddenly start getting data:
        verifyNoDataSub "owner" "sub" fooNs (root :/ bar1) [someWv WtInt32 16]

        -- Invalid PostDefinition and Definition IDs:
        badSub bazPdn
        badSub bazDn

        -- And again check that once the owner does define these, we haven't got
        -- registered subscriptions already:
        sendFwd $ ClientData "owner" $ SomeTrDigest $
          define baz strDef $
          postDefine baz pd $
          trpdEmpty fooNs
        verifyNoPdSub "owner" "sub" fooNs bazPdn
        verifyNoTySub "owner" "sub" fooNs bazDn

    it "handles legitimate subscriptions" $ testRelay mempty $ do
      -- Make an API with an owner:
      sendFwd $ ClientConnect "Owner" "owner"
      sendFwd $ ClientData "owner" $ SomeTrDigest $ simpleClaim foo
      expect [emptyRootDig "owner", nsExists fooNs "owner"]

      -- Connect a subscriber:
      sendFwd $ ClientConnect "Subscriber" "sub"
      expect [nsExists fooNs "sub"]

      -- Subscribe to some data and check data and type subscriptions:
      withSubscription fooNs root "sub" $ do
        expect [ ServerData "sub" $ SomeFrDigest $ (frcudEmpty fooNs)
          { frcudDefs = Map.singleton fooDn $ OpDefine intDef
          , frcudTyAssigns = Map.singleton Root (fooDn, Editable)
          , frcudData = AL.singleton Root $ ConstChange Nothing [someWv WtInt32 12]
          }]
        verifyDataSub "owner" "sub" fooNs Root [someWv WtInt32 13]
        verifyTySub "owner" "sub" fooNs fooDn

      -- After unsubscribing from the data, check that
      -- a) The data subscription is terminated
      verifyNoDataSub "owner" "sub" fooNs Root [someWv WtInt32 14]
      -- b) The type subscription remains
      verifyTySub "owner" "sub" fooNs fooDn

      -- Ditch the residual type subscription
      unsubscribe fooNs fooDn "sub"
      verifyNoTySub "owner" "sub" fooNs fooDn

      -- Test regulare sub/unsub cycle for Definitions
      withSubscription fooNs fooDn "sub" $ do
        expect [ ServerData "sub" $ SomeFrDigest $ (frcudEmpty fooNs)
          { frcudDefs = Map.singleton fooDn $ OpDefine intDef }]
        verifyTySub "owner" "sub" fooNs fooDn
      verifyNoTySub "owner" "sub" fooNs fooDn

      -- Test sub/unsub cycle for PostDefinitions too
      let pd = postDef' "fooy" [(x, ttInt64 unbounded)]
      sendFwd $ ClientData "owner" $ SomeTrDigest $ postDefine foo pd $ trpdEmpty fooNs
      withSubscription fooNs fooPdn "sub" $ do
        expect [ ServerData "sub" $ SomeFrDigest $ (frcudEmpty fooNs)
         { frcudPostDefs = Map.singleton fooPdn $ OpDefine pd }]
        verifyPdSub "owner" "sub" fooNs fooPdn
      verifyNoPdSub "owner" "sub" fooNs fooPdn


    it "should not send empty updates on empty subscriptions" $
      testRelay mempty $ do
        sendFwd $ ClientConnect "Subscriber" "sub"
        expect [emptyRootDig "sub"]

        sendFwd $ ClientData "sub" $ SomeTrDigest $ trcsdEmpty
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
        let pd = postDef' "fooy" [(x, ttInt64 unbounded)]
        sendFwd $ ClientData "owner" $ SomeTrDigest $
          postDefine foo pd $ simpleClaim foo
        expectSet $ nsExists fooNs <$> ["owner", "sub"]

        -- PostDefinition, Definition and data don't exist:
        checkAlreadyUnsub $ barPdn
        checkAlreadyUnsub $ barDn
        checkAlreadyUnsub $ Root :/ (bar :: DataName)

        -- PostDefinition, Definition and data exist, but never subscribed
        checkAlreadyUnsub fooPdn
        checkAlreadyUnsub fooDn
        checkAlreadyUnsub root

        -- PostDefinition, Definition and data exist, previous subscription
        withSubscription fooNs fooPdn "sub" $ do
          expect [ServerData "sub" $ SomeFrDigest $ (frcudEmpty fooNs)
            { frcudPostDefs = Map.singleton fooPdn $ OpDefine pd }]
        checkAlreadyUnsub fooPdn
        verifyNoPdSub "owner" "sub" fooNs fooPdn

        withSubscription fooNs fooDn "sub" $ do
          expect [ServerData "sub" $ SomeFrDigest $ (frcudEmpty fooNs)
            { frcudDefs = Map.singleton fooDn $ OpDefine intDef }]
        checkAlreadyUnsub fooDn
        verifyNoTySub "owner" "sub" fooNs fooDn

        withSubscription fooNs root "sub" $ do
          expect [ ServerData "sub" $ SomeFrDigest $ (frcudEmpty fooNs)
            { frcudDefs = Map.singleton fooDn $ OpDefine intDef
            , frcudTyAssigns = Map.singleton Root (fooDn, Editable)
            , frcudData = AL.singleton Root $
              ConstChange Nothing [someWv WtInt32 12]
            }]
        checkAlreadyUnsub root
        verifyNoDataSub "owner" "sub" fooNs root [someWv WtInt32 15]


    let sub_owner_preamble = do
          sendFwd $ ClientConnect "Owner" "owner"
          sendFwd $ ClientConnect "Subscriber" "sub"
          expect $ emptyRootDig <$> ["owner", "sub"]

          sendFwd $ ClientData "owner" $ SomeTrDigest $ structClaim foo
          expectSet $ nsExists fooNs <$> ["owner", "sub"]

    it "unsubscribes clients on client disconnect" $ testRelay mempty $ do
      -- i.e. after client disconnects, the relay never tries to send another
      -- message to that client.
      sub_owner_preamble
      subscribe fooNs fooDn "sub"
      expect [ ServerData "sub" $ SomeFrDigest $ (frcudEmpty fooNs)
        { frcudDefs = Map.singleton fooDn $ OpDefine $
          structDef' "test" [(foo, [n|texty|]), (bar, [n|inty|])]
        }]

      sendFwd $ ClientDisconnect "sub"
      verifyNoTySub "owner" "sub" fooNs fooDn
      sendFwd $ ClientDisconnect "owner"

    it "unsubscribes clients on relay disconnect" $ testRelay mempty $ do
      -- i.e. after the relay disconnects a client, the relay never tries to
      -- send another message to that client.
      sub_owner_preamble
      subscribe fooNs fooDn "sub"
      expect [ ServerData "sub" $ SomeFrDigest $ (frcudEmpty fooNs)
        { frcudDefs = Map.singleton fooDn $ OpDefine $
          structDef' "test" [(foo, [n|texty|]), (bar, [n|inty|])]
        }]

      -- We have to trigger a client disconnect by being a bad provider:
      sendFwd $ ClientData "sub" $ SomeTrDigest $ simpleClaim foo
      expectFrped "sub" [(NamespaceError fooNs, "Already owned")]

      verifyNoTySub "owner" "sub" fooNs fooDn

    it "unsubscribes clients on owner disconnect and disowns" $
      -- Also that the client gets notified of the deletion of the owner's data.
      testRelay mempty $ let textyTn = [n|texty|] :: DefName in do
        sub_owner_preamble
        subscribe fooNs textyTn "sub"
        expect [ ServerData "sub" $ SomeFrDigest $ (frcudEmpty fooNs)
          { frcudDefs = Map.singleton textyTn $ OpDefine strDef }]

        sendFwd $ ClientDisconnect "owner"
        expectSet
          [ ServerData "sub" $ SomeFrDigest $
            frcsdEmpty {frcsdTypeUnsubs = Mos.singleton fooNs textyTn}
          , nsCease fooNs "sub"
          ]

    it "doesn't forward empty subscriber bundles to owner" $
      testRelay mempty $ do
        sub_owner_preamble
        sendFwd $ ClientData "sub" $ SomeTrDigest $ trcudEmpty fooNs

    it "validates subscriber mutations and forwards valid" $
      testRelay mempty $ do
        sub_owner_preamble
        sendFwd $ ClientData "sub" $ SomeTrDigest $
          subSet (Root :/ foo) [someWv WtString "hello"] $
          subSet (Root :/ bar) [someWv WtInt32 3, someWv WtInt32 4] $
          trcudEmpty fooNs
        expect [ServerData "owner" $ SomeFrDigest $ (frpdEmpty fooNs)
          { frpdData = AL.singleton (Root :/ foo) $
            ConstChange Nothing [someWv WtString "hello"]
          }]
        expectSubErr "sub" (PathError $ Root :/ bar) "Mismatched numbers"

    it "validates owner claims" $ testRelay mempty $ do
      sendFwd $ ClientConnect "Owner" "owner"
      expect [emptyRootDig "owner"]

      sendFwd $ ClientData "owner" $ SomeTrDigest $
        ownerSet root [someWv WtString "Not an int"] $
        define foo intDef $
        trpdEmpty fooNs
      expectFrped "owner" [(PathError Root, "Cannot produce int32")]

    it "validates owner mutations" $ testRelay mempty $ do
      sendFwd $ ClientConnect "Owner" "owner"
      sendFwd $ ClientData "owner" $ SomeTrDigest $ simpleClaim foo
      expect [emptyRootDig "owner", nsExists fooNs "owner"]

      sendFwd $ ClientData "owner" $ SomeTrDigest $
        ownerSet root [someWv WtString "Not an int"] $ trpdEmpty fooNs
      expectFrped "owner" [(PathError Root, "Cannot produce int32")]

    it "rejects orphan data" $ testRelay mempty $ do
      sendFwd $ ClientConnect "Owner" "owner"
      expect [emptyRootDig "owner"]
      sendFwd $ ClientData "owner" $ SomeTrDigest $
        ownerSet (Root :/ baz) [someWv WtString "Orphan"] $ structClaim foo
      expectFrped "owner" [(PathError (Root :/ baz), "Invalid struct child")]

  where
    rootDig client = ServerData client . SomeFrDigest . Frcrd
    emptyRootDig client = rootDig client mempty
    nsExists ns client = rootDig client $ AL.singleton ns $ SoAfter Nothing
    nsCease ns client = rootDig client $ AL.singleton ns $ SoAbsent
    err client i msg = errs client $ Mol.singleton i msg
    errs client = ServerData client . SomeFrDigest . Frped

    verifySub
      :: Subscribe entity
      => String -> String -> Namespace -> EntityId entity -> Mutator entity
      -> TestProtocol ()
    verifySub ownerId subId ns ident mutation = do
      existing <- retrieve ns ident
      let new = mutate mutation existing
      sendFwd $ ClientData ownerId $ SomeTrDigest $ mkTrpd ns ident new
      expect [ServerData subId $ SomeFrDigest $ mkFrcud ns ident new]
      sendFwd $ ClientData ownerId $ SomeTrDigest $ mkTrpd ns ident existing
      expect [ServerData subId $ SomeFrDigest $ mkFrcud ns ident existing]

    verifyDataSub = verifySub @[SomeWireValue]
    verifyTySub o s n i = verifySub @SomeDefinition o s n i "changed doc"
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
      sendFwd $ ClientData ownerId $ SomeTrDigest $ mkTrpd ns ident new
      nothingWaiting
      sendFwd $ ClientData ownerId $ SomeTrDigest $ mkTrpd ns ident existing
      nothingWaiting

    verifyNoDataSub = verifyNoSub @[SomeWireValue]
    verifyNoTySub o s n i = verifyNoSub @SomeDefinition o s n i "changed doc"
    verifyNoPdSub o s n i = verifyNoSub @PostDefinition o s n i "changed doc"

    intDef = tupleDef'
      "A single unbounded integer" [(x, ttInt32 unbounded)] Nothing
    strDef = tupleDef' "Any string" [(x, ttString "")] Nothing

    simpleClaim name =
      ownerSet Root [someWv WtInt32 12] $
      define name intDef $
      trpdEmpty $ castName name

    structClaim name =
      ownerSet (Root :/ bar) [someWv WtInt32 2] $
      ownerSet (Root :/ foo) [someWv WtString "one"] $
      define name (structDef' "test"
        [ (foo, [n|texty|])
        , (bar, [n|inty|])
        ]) $
      define [n|inty|] intDef $
      define [n|texty|] strDef $
      trpdEmpty $ castName name

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
  Void (ClientEvent String SomeTrDigest)
  Void (Either (Map Namespace String) (ServerEvent String SomeFrDigest))
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
  -> Protocol Void x1 x2 (Either x3 (ServerEvent String SomeFrDigest)) IO ()
expectFrped clientId expected = do
    onNext isRight handleFrped
    expect [ServerDisconnect clientId]
  where
    handleFrped (Right (ServerData i (SomeFrDigest frped@(Frped {})))) = do
      i `shouldBe` clientId
      forM_ expected $ \(ei, expMsg) ->
        case Mol.lookup ei $ frpedErrors frped of
          [] -> expectationFailure $
            printf "%s not in errors:\n%s" (show ei) (show $ frpedErrors frped)
          msgs -> mapM_ (`shouldContain` expMsg) $ Text.unpack <$> msgs
    handleFrped _ = expectationFailure "Not an error digest"

-- FIXME: might this be better is we had explicit error digests for the client,
-- rather than intermingling them in the data updates?
-- | Expect a subscriber to have been given an error
expectSubErr
  :: String -> DataErrorIndex -> String
  -> Protocol Void v1 v2 (Either x3 (ServerEvent String SomeFrDigest)) IO ()
expectSubErr clientId ei expMsg = onNext isRight handleFrcud
  where
    handleFrcud (Right (ServerData i (SomeFrDigest frcud@(Frcud {})))) = do
      i `shouldBe` clientId
      case Mol.lookup ei $ frcudErrors frcud of
        [] -> expectationFailure $
          printf "%s not in errors:\n%s" (show ei) (show $ frcudErrors frcud)
        msgs -> mapM_ (`shouldContain` expMsg) $ Text.unpack <$> msgs
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
      Right (ServerData ident' (SomeFrDigest (Frcrd _))) -> ident' == ident
      _ -> False

-- | Fail if there is are unahandled messages from the relay waiting for us.
nothingWaiting :: TestProtocol ()
nothingWaiting = withWitness "nothing waiting" $ return ()

unOpDefine :: DefOp a -> Maybe a
unOpDefine = \case
  OpDefine a -> Just a
  _ -> Nothing

unConstChange :: DataChange -> Maybe [SomeWireValue]
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
  _sendSub op ns name clientId = sendFwd $ ClientData clientId $ SomeTrDigest $
    mkTrcsd ns name op

instance Subscribe PostDefinition where
  type EntityId PostDefinition = PostDefName
  mkTrcsd ns name op = mempty {trcsdPostTys = Map.singleton (ns, name) op}
  mkFrcsd ns name = mempty {frcsdPostTypeUnsubs = Mos.singleton ns name}
  mkTrpd ns name ent = (trpdEmpty ns)
    { trpdPostDefs = Map.singleton name $ OpDefine ent }
  mkFrcud ns name ent = (frcudEmpty ns)
    { frcudPostDefs = Map.singleton name $ OpDefine ent }
  _repr _ = "post def"
  extractEntity name = Map.lookup name . frcudPostDefs >=> unOpDefine
  type Mutator PostDefinition = Text
  mutate doc pd = pd { postDefDoc = doc }

instance Subscribe SomeDefinition where
  type EntityId SomeDefinition = DefName
  mkTrcsd ns name op = mempty {trcsdTys = Map.singleton (ns, name) op}
  mkFrcsd ns name = mempty {frcsdTypeUnsubs = Mos.singleton ns name}
  mkTrpd ns name ent = (trpdEmpty ns)
    { trpdDefs = Map.singleton name $ OpDefine ent }
  mkFrcud ns name ent = (frcudEmpty ns)
    { frcudDefs = Map.singleton name $ OpDefine ent }
  _repr _ = "def"
  extractEntity name = Map.lookup name . frcudDefs >=> unOpDefine
  type Mutator SomeDefinition = Text
  mutate doc (SomeDefinition def) = case def of
    ArrayDef {} -> SomeDefinition $ def {arrDefDoc = doc}
    StructDef {} -> SomeDefinition $ def {strDefDoc = doc}
    TupleDef {} -> SomeDefinition $ def {tupDefDoc = doc}

instance Subscribe [SomeWireValue] where
  type EntityId [SomeWireValue] = Path
  mkTrcsd ns path op = mempty {trcsdData = Map.singleton (ns, path) op}
  mkFrcsd ns path = mempty {frcsdDataUnsubs = Mos.singleton ns path}
  mkTrpd ns name ent = (trpdEmpty ns)
    { trpdData = AL.singleton name $ ConstChange Nothing ent }
  mkFrcud ns name ent = (frcudEmpty ns)
    {frcudData = AL.singleton name $ ConstChange Nothing ent }
  _repr _ = "data"
  extractEntity name = AL.lookup name . frcudData >=> unConstChange
  type Mutator [SomeWireValue] = [SomeWireValue]
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
  expect [ServerData clientId $ SomeFrDigest $ mkFrcsd ns name]

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
    Right (ServerData _ (SomeFrDigest frcud@(Frcud {}))) <- waitUntil isRight
    return $ fromMaybe (error $ "missing existing " ++ etName) $
        extractEntity name frcud

define :: DefName -> SomeDefinition -> TrpDigest -> TrpDigest
define name def trpd = trpd
  { trpdDefs = Map.insert name (OpDefine def) $
    trpdDefs trpd }

postDefine :: PostDefName -> PostDefinition -> TrpDigest -> TrpDigest
postDefine name def trpd = trpd
  { trpdPostDefs = Map.insert name (OpDefine def) $
    trpdPostDefs trpd }

ownerSet :: Path -> [SomeWireValue] -> TrpDigest -> TrpDigest
ownerSet path values trpd = trpd
  { trpdData = AL.insert path (ConstChange Nothing values) $ trpdData trpd }

subSet :: Path -> [SomeWireValue] -> TrcUpdateDigest -> TrcUpdateDigest
subSet path values trcud = trcud
  { trcudData = AL.insert path (ConstChange Nothing values) $ trcudData trcud }

foo, bar, baz, x :: Name nr
foo = [n|foo|]; bar = [n|bar|]; baz = [n|baz|]; x = [n|x|]

fooNs, barNs, bazNs :: Namespace
fooNs = foo; barNs = bar; bazNs = baz

fooDn, barDn, bazDn :: DefName
fooDn = foo; barDn = bar; bazDn = baz

fooPdn, barPdn, bazPdn :: PostDefName
fooPdn = foo; barPdn = bar; bazPdn = baz

-- | A non-polymorphic root path
root :: Path
root = Root

arrayDef' :: Text -> DefName -> Editability -> SomeDefinition
arrayDef' doc dn ed = arrayDef doc Nothing dn ed

structDef' :: Text -> [(DataName, DefName)] -> SomeDefinition
structDef' doc tys = structDef doc $ AL.unsafeMkAssocList $
  fmap (,Editable) <$> tys

tupleDef'
  :: Text -> [(TupMemberName, SomeTreeType)] -> InterpolationLimit
  -> SomeDefinition
tupleDef' doc tys il = tupleDef doc (AL.unsafeMkAssocList tys) il

postDef' :: Text -> [(PostArgName, SomeTreeType)] -> PostDefinition
postDef' doc tys = PostDefinition doc (AL.unsafeMkAssocList $ fmap pure <$> tys)

defMap :: Ord (DefKey def) => [(DefKey def, def)] -> DefMap def
defMap = Map.fromList

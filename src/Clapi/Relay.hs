{-# LANGUAGE OverloadedStrings #-}
module Clapi.Relay where

import Control.Monad.Fail (MonadFail)
import Control.Monad.State (State, state, runState)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Void (Void)
import Data.Maybe (mapMaybe)
import Data.Either (partitionEithers, rights)

import Clapi.Types (
    CanFail, OwnerUpdateMessage(..), TreeUpdateMessage(..),
    DataUpdateMessage(..), UMsgError(..), UMsg(..), ClapiValue(..), Time(..),
    Enumerated(..), toClapiValue)
import Clapi.Path (Path, Name, root)
import qualified Clapi.Tree as Tree
import Clapi.Tree (_getSites)
import Clapi.Valuespace (
    Valuespace, vsSet, vsAdd, vsRemove, vsClear, vsAssignType,
    vsDelete, Unvalidated, unvalidate, Validated, vsValidate, MonadErrorMap,
    vsGetTree, VsDelta, vsDiff, getType, Liberty(Cannot))
import Clapi.NamespaceTracker (maybeNamespace, Request(..), Response(..))
import Clapi.Protocol (Protocol, waitThen, sendRev)
import Path.Parsing (toText)

applyMessage :: (MonadFail m) => Valuespace Unvalidated -> OwnerUpdateMessage -> m (Valuespace Unvalidated)
applyMessage vs msg = case msg of
    (Right (UMsgSet p t v i a s)) -> vsSet a i v p s t vs
    (Right (UMsgAdd p t v i a s)) -> vsAdd a i v p s t vs
    (Right (UMsgRemove p t a s)) -> vsRemove a p s t vs
    (Right (UMsgClear p t a s)) -> vsClear a p s t vs
    (Right (UMsgSetChildren p c a)) -> undefined -- FIXME: backing implementation
    (Left (UMsgAssignType p tp)) -> return $ vsAssignType p tp vs
    (Left (UMsgDelete p)) -> vsDelete p vs

applyMessages :: Valuespace Unvalidated -> [OwnerUpdateMessage] -> MonadErrorMap (Valuespace Unvalidated)
applyMessages mvvs = applySuccessful [] mvvs
  where
    applySuccessful :: [(Path, String)] -> Valuespace Unvalidated -> [OwnerUpdateMessage] -> MonadErrorMap (Valuespace Unvalidated)
    applySuccessful errs vs [] = (Map.fromList errs, vs)
    applySuccessful errs vs (m:ms) = case canFailApply vs m of
        (Left es) -> applySuccessful ((uMsgPath m, es):errs) vs ms
        (Right vs') -> applySuccessful errs vs' ms
    canFailApply :: Valuespace Unvalidated -> OwnerUpdateMessage -> CanFail (Valuespace Unvalidated)
    canFailApply = applyMessage

treeDeltaToMsg :: Path -> Tree.TreeDelta [ClapiValue] -> OwnerUpdateMessage
treeDeltaToMsg p td = case td of
    Tree.Delete -> Left $ UMsgDelete p
    (Tree.SetChildren c) -> Right $ UMsgSetChildren p c Nothing -- FIXME: attribution!
    (Tree.Clear t s a) -> Right $ UMsgClear p t a s
    (Tree.Remove t s a) -> Right $ UMsgRemove p t a s
    (Tree.Add t v i s a) -> Right $ UMsgAdd p t v i s a
    (Tree.Set t v i s a) -> Right $ UMsgSet p t v i s a

deltaToMsg :: VsDelta -> OwnerUpdateMessage
deltaToMsg (p, d) = either (Left . UMsgAssignType p) (treeDeltaToMsg p) d

modifyRootTypePath :: Valuespace Validated -> Valuespace Unvalidated -> Valuespace Unvalidated
modifyRootTypePath vs vs' = if rootType' == rootType then vs' else vs''
  where
    rootTypePath = moe "root has no type" $ getType vs' root
    rootTypeNode = loe "root type path missing" rootTypePath $ vsGetTree vs'
    (a, mtp) = loe "root type missing zero" z $ loe "root type missing global site" Nothing $ _getSites rootTypeNode
    (i, rootType) = moe "root type deleted" mtp
    rootType' = foldl updateRootType rootType $ mapMaybe nsChange $ vsDiff vs vs'
    vs'' = moe "root type set failed" $ vsSet a i rootType' rootTypePath Nothing z vs'
    z = Time 0 0
    loe s k m = moe s $ Map.lookup k m
    moe s m = maybe (error s) id m

handleMutationMessages ::
    Valuespace Validated ->
    [OwnerUpdateMessage] ->
    ([OwnerUpdateMessage], [UMsgError], Valuespace Validated)
handleMutationMessages vs msgs = (vcMsgs, errMsgs, vvs)
  where
    errs = Map.union aErrs vErrs
    (aErrs, vs') = applyMessages (unvalidate vs) msgs
    vs'' = modifyRootTypePath vs vs'
    (vErrs, vvs) = vsValidate vs''
    errMsgs = map (\(p, es) -> UMsgError p (T.pack es)) (Map.assocs errs)
    vcMsgs = dmsgs vs vvs
    dmsgs v v' = map deltaToMsg $ vsDiff v v'

handleOwnerMessages ::
    [OwnerUpdateMessage] ->
    Valuespace Validated ->
    (Either [UMsgError] [OwnerUpdateMessage], Valuespace Validated)
handleOwnerMessages msgs vs = (rmsgs, rvs)
  where
    -- FIXME: handle owner initiated error messages
    rvs = if errored then vs else vvs
    errored = not $ null errMsgs
    (vcMsgs, errMsgs, vvs) = handleMutationMessages vs msgs
    fillerErrPaths = Set.toList $ let sop ms = Set.fromList $ map uMsgPath ms in Set.difference (sop msgs) (sop errMsgs)
    fillerErrs = map (flip UMsgError "rejected due to other errors") fillerErrPaths
    rmsgs = case errored of
        True -> Left $ errMsgs ++ fillerErrs
        False -> Right $ vcMsgs

handleOwnerMessagesS :: [OwnerUpdateMessage] -> State (Valuespace Validated) (Either [UMsgError] [OwnerUpdateMessage])
handleOwnerMessagesS msgs = state (handleOwnerMessages msgs)

-- Technically this returns [DataUpdateMessage] for vsMsgs
handleClientMessages ::
    [Path] ->
    [DataUpdateMessage] ->
    Valuespace Validated ->
    (([UMsgError], [OwnerUpdateMessage], [OwnerUpdateMessage]), Valuespace Validated)
handleClientMessages getPaths updates vs = ((subErrs ++ vsErrs, getMs, vsMsgs), vs)
  where
    (subErrs, getMs) = fmap concat $ partitionEithers $ map handleGet $ zip (map nodeInfo getPaths) getPaths
    handleGet ((Nothing, Nothing), p) = Left $ UMsgError p "Not found"
    handleGet ((Just tp, Just n), p) = Right $ (Left $ UMsgAssignType p tp) : nodeMsgs p n
    nodeInfo p = (getType vs p, Map.lookup p $ vsGetTree vs)
    nodeMsgs p n = map (treeDeltaToMsg p) $ rightOrDie $ Tree.nodeDiff mempty n
    rightOrDie (Right x) = x
    (vsMsgs, vsErrs, _) = handleMutationMessages vs (map Right updates)

handleClientMessagesS ::
    [Path] ->
    [DataUpdateMessage] ->
    State (Valuespace Validated) ([UMsgError], [OwnerUpdateMessage], [OwnerUpdateMessage])
handleClientMessagesS paths msgs = state (handleClientMessages paths msgs)

handleMessages :: Valuespace Validated -> Request -> (Response, Valuespace Validated)
handleMessages vs (ClientRequest getPaths updates) = runState hc vs
  where
    hc = do
        (errs, gets, vmsgs) <- handleClientMessagesS getPaths updates
        return $ ClientResponse gets errs (rights vmsgs)  -- FIXME: rights is a fudge
handleMessages vs (OwnerRequest updates) = runState ho vs
  where
    ho = do
        e <- handleOwnerMessagesS updates
        return $ either BadOwnerResponse GoodOwnerResponse e

relay :: Monad m => Valuespace Validated -> Protocol (i, Request) Void (i, Response) Void m ()
relay vs = waitThen fwd (const $ error "message from the void")
  where
    fwd (i, req) = do
        let (resp, vs') = handleMessages vs req
        sendRev (i, resp)
        relay vs'

data NsChange =
    NsAssign Name Path
  | NsRemove Name

nsChange :: VsDelta -> Maybe NsChange
nsChange (p, Left tp) = maybeNamespace (flip NsAssign tp) p
nsChange (p, Right Tree.Delete) = maybeNamespace NsRemove p
nsChange _ = Nothing

updateRootType :: [ClapiValue] -> NsChange -> [ClapiValue]
updateRootType [lib, doc, ClList names, ClList types, ClList libs] m = lib:doc:tinfo
  where
    tinfo = map ClList $ case m of
        (NsAssign n tp) -> [c n:names, cp tp:types, cannot:libs]
        (NsRemove n) -> l3 . unzip3 $ filter (nameIsnt n) $ zip3 names types libs
    l3 (ns, ts, ls) = [ns, ts, ls]
    nameIsnt n (n', _, _) = n' /= c n
    c = ClString
    cp = c . toText
    cannot = toClapiValue $ Enumerated Cannot

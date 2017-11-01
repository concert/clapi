{-# LANGUAGE OverloadedStrings #-}
module Clapi.Relay where

import Data.List (partition)
import Control.Monad.Fail (MonadFail)
import Control.Monad.State (MonadState, StateT(..), evalStateT, runStateT, get, modify, put, State, state, runState)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Text.Printf (printf)
import Data.Void (Void)

import Control.Lens (_1, _2)
import Pipes (lift)
import Pipes.Core (Client, request)

import Clapi.Util (eitherFail)
import Clapi.Types (CanFail, Message(..), ClapiMethod(..), ClapiValue)
import Clapi.Path (Path)
import qualified Clapi.Tree as Tree
import Clapi.Validator (Validator, validate)
import Clapi.Valuespace (
    VsTree, Valuespace(..), vsSet, vsAdd, vsRemove, vsClear, vsAssignType,
    vsDelete, Unvalidated, unvalidate, Validated, vsValidate, MonadErrorMap,
    vsGetTree, VsDelta, vsDiff, getType)
import Clapi.NamespaceTracker (stateL, stateL', Ownership(..))
import Clapi.Server (User)
import Clapi.Protocol (Protocol, waitThen, sendRev)
import Data.Maybe.Clapi (note)


-- failyModify :: (MonadState s m) => (s -> CanFail s) -> m (Maybe String)
-- failyModify f =
--   do
--     result <- get >>= return . f
--     -- FIXME: rework into MonadFail or MonadError?
--     case result of
--         Left msg -> return $ Just msg
--         Right s' -> put s' >> return Nothing


-- -- bar :: (Monad m) => Message -> StateT (VsTree) m (Maybe Message)
-- -- bar (MsgSet p t cvs i a s) = failyModify (treeSet a i cvs p s t) >>= return . fmap (blah p)
-- -- bar (MsgAdd p t cvs i a s) = failyModify (treeAdd a i cvs p s t) >>= return . fmap (blah p)
-- -- bar (MsgRemove p t a s) = failyModify (treeRemove a p s t) >>= return . fmap (blah p)
-- -- bar (MsgClear p t a s) = failyModify (treeClear a p s t) >>= return . fmap (blah p)
-- -- -- FIXME: this currently can't fail because the type doesn't have to be in the
-- -- -- tree at init time...
-- -- bar (MsgAssignType p tp) = modify (treeInitNode p tp) >> return Nothing
-- -- bar (MsgDelete p) = failyModify (treeDelete p) >>= return . fmap (blah p)
-- -- bar (MsgChildren p ns) = failyModify (treeSetChildren p ns) >>= return . fmap (blah p)
-- -- bar _ = undefined

-- blah :: Path -> String -> Message
-- blah p = MsgError p . T.pack


-- -- FIXME: this stuff probably belongs in Valuespace cf here
-- getValidators ::
--     (MonadFail m) => Path -> Valuespace -> m [Validator]
-- getValidators np (Valuespace t _ _ defs) =
--   do
--     tp <- treeGetType np t
--     f $ definitionValidators <$> Map.lookup tp defs
--   where
--     f = note (printf "No definition cached for path %s" (show np))

-- validateArgs ::
--     (MonadFail m) => Path -> [ClapiValue] -> Valuespace -> m ()
-- validateArgs np cvs v@(Valuespace t _ dmap) = do
--     vs <- getValidators np v
--     eitherFail $ validate t vs cvs

-- stateyValidate ::
--     (MonadFail m) => Path -> [ClapiValue] -> StateT Valuespace m ()
-- stateyValidate p cvs = get >>= validateArgs p cvs


-- foo :: (Monad m) => Message -> StateT Valuespace m (Maybe Message)
-- foo (MsgSet p t cvs i a s) = undefined
-- foo (MsgAdd p t cvs i a s) = undefined
-- foo (MsgRemove p t a s) = undefined
-- foo (MsgClear p t a s) = undefined
-- foo (MsgAssignType p tp) = undefined
-- foo (MsgDelete p) = undefined
-- foo (MsgChildren p ns) = undefined
-- foo _ = undefined


-- relay ::
--     (Monad m) => Valuespace -> (User, [Om]) -> Client [Om] (User, [Om]) m r
-- relay v x = evalStateT (_relay x) v

-- _relay ::
--     (Monad m) => (User, [Om]) ->
--     StateT Valuespace (Client [Om] (User, [Om]) m) r
-- _relay (u, oms) = do
--     initVs <- get
--     (oms', (persistantVs, _)) <-
--         runStateT (processOms u oms) (initVs, initVs)
--     put persistantVs
--     lift (request oms') >>= _relay

-- processOms ::
--     (Monad m) => User -> [Om] -> StateT (Valuespace, Valuespace) m [Om]
-- processOms u oms =
--   do
--     undefined

applyMessage :: (MonadFail m) => Valuespace v -> Message -> m (Valuespace Unvalidated)
applyMessage vs msg = case msg of
    (MsgSet p t v i a s) -> vsSet a i v p s t vs
    (MsgAdd p t v i a s) -> vsAdd a i v p s t vs
    (MsgRemove p t a s) -> vsRemove a p s t vs
    (MsgClear p t a s) -> vsClear a p s t vs
    (MsgAssignType p tp) -> return $ vsAssignType p tp vs
    (MsgDelete p) -> vsDelete p vs
    (MsgChildren p c) -> undefined -- FIXME: backing implementation
    (MsgSubscribe _) -> return $ unvalidate vs
    (MsgUnsubscribe _) -> return $ unvalidate vs
    (MsgError _ _) -> return $ unvalidate vs

applyMessages :: Valuespace v -> [Message] -> MonadErrorMap (Valuespace Unvalidated)
applyMessages mvvs = applySuccessful [] mvvs
  where
    applySuccessful :: [(Path, String)] -> Valuespace v -> [Message] -> MonadErrorMap (Valuespace Unvalidated)
    applySuccessful errs vs [] = (Map.fromList errs, unvalidate vs)
    applySuccessful errs vs (m:ms) = case canFailApply vs m of
        (Left es) -> applySuccessful ((msgPath' m, es):errs) vs ms
        (Right vs') -> applySuccessful errs vs' ms
    canFailApply :: Valuespace v -> Message -> CanFail (Valuespace Unvalidated)
    canFailApply = applyMessage

treeDeltaToMsg :: Path -> Tree.TreeDelta [ClapiValue] -> Message
treeDeltaToMsg p td = case td of
    Tree.Delete -> MsgDelete p
    (Tree.SetChildren c) -> MsgChildren p c
    (Tree.Clear t s a) -> MsgClear p t a s
    (Tree.Remove t s a) -> MsgRemove p t a s
    (Tree.Add t v i s a) -> MsgAdd p t v i s a
    (Tree.Set t v i s a) -> MsgSet p t v i s a

deltaToMsg :: VsDelta -> Message
deltaToMsg (p, d) = either (MsgAssignType p) (treeDeltaToMsg p) d

-- This is kinda fanoutable rather than routable
-- Also not sure about the either, should there be a strategy type instead?
data RoutableMessage i = RoutableMessage {rmRoute :: Either i Ownership, rmMsg :: Message}

handleMutationMessages :: Valuespace Validated -> [Message] -> ([Message], [Message], Valuespace Validated)
handleMutationMessages vs msgs = (vcMsgs, errMsgs, vvs)
  where
    errs = Map.union aErrs vErrs
    (aErrs, vs') = applyMessages vs msgs
    (vErrs, vvs) = vsValidate vs'
    errMsgs = map (\(p, es) -> MsgError p (T.pack es)) (Map.assocs errs)
    vcMsgs = map (\d -> deltaToMsg d) $ vsDiff vs vvs

handleOwnerMessages :: i -> [Message] -> Valuespace Validated -> ([RoutableMessage i], Valuespace Validated)
handleOwnerMessages respondTo msgs vs = (rmsgs, rvs)
  where
    -- FIXME: handle owner initiated error messages
    rvs = if errored then vs else vvs
    errored = not $ null errMsgs
    (vcMsgs, errMsgs, vvs) = handleMutationMessages vs msgs
    fillerErrPaths = Set.toList $ let sop ms = Set.fromList $ map msgPath' ms in Set.difference (sop msgs) (sop errMsgs)
    fillerErrs = map (\p -> MsgError p "rejected due to other errors") fillerErrPaths
    rmsgs = case errored of
        True -> map (\m -> RoutableMessage (Left respondTo) m) (errMsgs ++ fillerErrs)
        False -> map (\m -> RoutableMessage (Right Owner) m) vcMsgs

type VsHandlerS i = i -> [Message] -> State (Valuespace Validated) [RoutableMessage i]

handleOwnerMessagesS :: VsHandlerS i
handleOwnerMessagesS respondTo msgs = state (handleOwnerMessages respondTo msgs)

handleClientMessages :: i -> [Message] -> Valuespace Validated -> ([RoutableMessage i], Valuespace Validated)
handleClientMessages respondTo msgs vs = (rmsgs, vs)
  where
    subMsgs = concatMap createMsgs subPaths
    createMsgs p = case nodeInfo p of
        (Just tp, Just n) -> [MsgAssignType p tp] ++ nodeMsgs p n
        (Nothing, Nothing) -> [MsgError p "no such path"]
    nodeInfo p = (getType vs p, Map.lookup p $ vsGetTree vs)
    nodeMsgs p n = map (treeDeltaToMsg p) $ rightOrDie $ Tree.nodeDiff mempty n
    subPaths = msgPath' <$> filter isSub msgs
    isSub (MsgSubscribe _) = True
    isSub _ = False
    rightOrDie (Right x) = x
    (vsMsgs, errMsgs, _) = handleMutationMessages vs msgs
    rmsgs = (map response $ errMsgs ++ subMsgs) ++ (map forwarded vsMsgs)
    forwarded = RoutableMessage (Right Client)
    response = RoutableMessage (Left respondTo)

handleClientMessagesS :: VsHandlerS i
handleClientMessagesS respondTo msgs = state (handleClientMessages respondTo msgs)

type Om = (Ownership, Message)

handleMessages :: Valuespace Validated -> i -> [Om] -> ([RoutableMessage i], Valuespace Validated)
handleMessages vs respondTo msgs = runState doHandle vs
  where
    (client, owner) = partition (\m -> fst m == Client) msgs
    doHandle = do
        oms <- handleOwnerMessagesS respondTo $ map snd owner
        cms <- handleClientMessagesS respondTo $ map snd client
        return $ oms ++ cms

relay :: Monad m => Valuespace Validated -> Protocol (i, [Om]) Void [RoutableMessage i] Void m ()
relay vs = waitThen fwd (const $ error "message from the void")
  where
    fwd (i, ms) = do
        let (ms', vs') = handleMessages vs i ms
        sendRev ms'
        relay vs'

{-# LANGUAGE OverloadedStrings #-}
module Clapi.Relay where

import Control.Monad.Fail (MonadFail)
import Control.Monad.State (MonadState, StateT(..), evalStateT, runStateT, get, modify, put)
import qualified Data.Map as Map
import qualified Data.Text as T
import Text.Printf (printf)

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
    vsGetTree, VsDelta, vsDiff)
import Clapi.NamespaceTracker (stateL, stateL')
import Clapi.Server (User)
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

deltaToMsg :: VsDelta -> Message
deltaToMsg (p, d) = case d of
    (Left tp) -> MsgAssignType p tp
    (Right td) -> case td of
        Tree.Delete -> MsgDelete p
        (Tree.SetChildren c) -> MsgChildren p c
        (Tree.Clear t s a) -> MsgClear p t a s
        (Tree.Remove t s a) -> MsgRemove p t a s
        (Tree.Add t v i s a) -> MsgAdd p t v i s a
        (Tree.Set t v i s a) -> MsgSet p t v i s a

handleOwnerMessages :: Valuespace Validated -> [Message] -> ([Message], Valuespace Validated)
handleOwnerMessages vs msgs = (rmsgs, rvs)
  where
    rvs = if errored then vs else vvs
    errored = errs == Map.empty
    errs = Map.union aErrs vErrs
    (aErrs, vs') = applyMessages vs msgs
    (vErrs, vvs) = vsValidate vs'
    -- FIXME: these don't cover every mentioned path
    errMsgs = map (\(p, es) -> MsgError p (T.pack es)) (Map.assocs errs)
    vcMsgs = map deltaToMsg $ vsDiff vs vvs
    rmsgs = if errored then errMsgs else vcMsgs

handleUnclaimedMessages :: Valuespace Validated -> [Message] -> ([Message], Valuespace Validated)
handleUnclaimedMessages vs msgs = if isValidClaim then handleOwnerMessages vs msgs else (errorAll, vs)
  where
    isValidClaim = errorStr == ""
    claims = filter isClaim msgs
    isClaim m = case m of
        (MsgAssignType p _) -> length p == 1
        _ -> False
    errorStr = case claims of
        [] -> "First bundle in unclaimed toplevel path does not contain claim"
        (m:[]) -> case Map.lookup (msgPath' m) (vsGetTree vs) of
            Nothing -> ""
            _ -> "Top level path already claimed"
        _ -> "Bundle claims multiple toplevel paths"
    errorAll = map (\m -> MsgError (msgPath' m) "Not within known toplevel path") msgs

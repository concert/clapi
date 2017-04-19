module Relay where

import Control.Monad.Fail (MonadFail)
import Control.Monad.State (MonadState, StateT(..), evalStateT, runStateT, get, modify, put)
import qualified Data.Map as Map
import qualified Data.Text as T
import Text.Printf (printf)

import Control.Lens (_1, _2)
import Pipes (lift)
import Pipes.Core (Client, request)

import Util (eitherFail)
import Types (CanFail, Message(..), ClapiMethod(..), ClapiValue)
import Path (Path)
import Tree (
    ClapiTree, Attributee, -- treeAdd, treeSet, treeRemove, treeClear, treeInitNode, treeDelete, treeSetChildren, treeGetType
    )
import Validator (Validator, validate)
import Valuespace (VsTree, Valuespace(..), definitionValidators)
import NamespaceTracker (Om, stateL, stateL')
import Server (User)
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

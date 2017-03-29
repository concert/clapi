module Relay where

import Control.Monad.State (MonadState, StateT(..), evalStateT, get, modify)

import Pipes (lift)
import Pipes.Core (Client, request)

import Types (ClapiMessage(..), ClapiMethod(..))
import Tree (ClapiTree, Attributee, treeAdd, treeSet, treeRemove, treeClear)
import NamespaceTracker (Om)
import Server (User)


failyModify :: (MonadState s m) => (s -> CanFail s) -> m (Maybe String)
failyModify f =
  do
    result <- get >>= return . f
    case result of
        Left msg -> Just msg
        Right s' -> put s' >> Nothing


foo :: (Monad m) => Om -> StateT (ClapiTree [ClapiValue]) m Om
foo (o, m) = undefined

bar :: (Monad m) => ClapiMessage -> StateT (ClapiTree [ClapiValue]) m ClapiMessage
bar (CMessage path Set args _) = treeSet (Just "fixme")
bar (CMessage path Add args _) = treeAdd (Just "fixme")
bar (CMessage path Remove args _) = treeRemove (Just "fixme")
bar (CMessage path Clear args _) = treeClear (Just "fixme")
bar _ = undefined


relay ::
    (Monad m) => ClapiTree [ClapiValue] -> (User, [Om]) -> Client [Om] (User, [Om]) m r
relay t x = evalStateT (_relay x) t

_relay ::
    (Monad m) => (User, [Om]) -> StateT (ClapiTree [ClapiValue]) (Client [Om] (User, [Om]) m) r
_relay (u, oms) = do
    (u', oms') <- lift $ request oms
    _relay (u', oms')

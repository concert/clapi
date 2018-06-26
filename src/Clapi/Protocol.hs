{-# LANGUAGE DeriveFunctor #-}

module Clapi.Protocol where -- (
--  Directed(..), Protocol, wait, sendFwd, sendRev, terminate, blimp, (<<->)
-- ) where

import Control.Concurrent.Async (async, cancel, concurrently_)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forever)
import Control.Monad.Trans (MonadIO(..))
import Control.Monad.Trans.Free
import qualified Data.Sequence as S
import Data.Void

data Directed a b = Fwd a | Rev b deriving (Show, Eq)

fromDirected :: (a -> c) -> (b -> c) -> Directed a b -> c
fromDirected f _ (Fwd a) = f a
fromDirected _ g (Rev b) = g b

data ProtocolF a a' b' b next =
    Wait (Directed a b -> next)
  | SendFwd a' next
  | SendRev b' next
  deriving (Functor)

-- instance (Show a', Show b') => Show (ProtocolF a a' b' b next) where
instance Show (ProtocolF a a' b' b next) where
  show (Wait _) = "wait"
  show (SendFwd _a' _) = "-> "-- ++ show a'
  show (SendRev _b' _) = "<- "-- ++ show b'

type Protocol a a' b' b m = FreeT (ProtocolF a a' b' b) m

wait :: (Monad m) => Protocol a a' b' b m (Directed a b)
wait = liftF $ Wait id

sendFwd :: (Monad m) => a' -> Protocol a a' b' b m ()
sendFwd a' = liftF $ SendFwd a' ()

sendRev :: (Monad m) => b' -> Protocol a a' b' b m ()
sendRev b' = liftF $ SendRev b' ()

composeRevBiased ::
    forall a1 a2 a3 b1 b2 b3 m.
    (Monad m) =>
    Protocol a1 a2 b3 b2 m () ->
    Protocol a2 a3 b2 b1 m () ->
    Protocol a1 a3 b3 b1 m ()
composeRevBiased protocol1 protocol2 = comp protocol1 mempty mempty protocol2
  where
    comp proto1 a2q b2q proto2 = FreeT $ do
        freeF1 <- runFreeT proto1
        freeF2 <- runFreeT proto2
        go freeF1 a2q b2q freeF2

    wrapFreeF ::
        FreeF (ProtocolF a a' b' b) () (Protocol a a' b' b m ()) ->
        Protocol a a' b' b m ()
    wrapFreeF = FreeT . return

    go ::
        (Monad m) =>
        FreeF (ProtocolF a1 a2 b3 b2) () (Protocol a1 a2 b3 b2 m ()) ->
        S.Seq a2 -> S.Seq b2 ->
        FreeF (ProtocolF a2 a3 b2 b1) () (Protocol a2 a3 b2 b1 m ()) ->
        m (FreeF (ProtocolF a1 a3 b3 b1) () (Protocol a1 a3 b3 b1 m ()))

    -- Right side is waiting and we have queued values:
    go freeF1 (a2 :< a2q) b2q (Free (Wait f2)) =
        runFreeT $ comp (wrapFreeF freeF1) a2q b2q (f2 (Fwd a2))

    -- Left side is waiting and we have queued values:
    go (Free (Wait f1)) a2q (b2 :< b2q) freeF2 =
        runFreeT $ comp (f1 (Rev b2)) a2q b2q (wrapFreeF freeF2)

    -- Both sides waiting:
    go (Free (Wait f1)) a2q b2q (Free (Wait f2)) =
        return $ Free (Wait wat)
      where
        wat directed = case directed of
          Fwd a1 -> comp (f1 (Fwd a1)) a2q b2q (wait >>= f2)
          Rev b1 -> comp (wait >>= f1) a2q b2q (f2 (Rev b1))

    -- Right sends to waiting left:
    go (Free (Wait f1)) _a2q _b2q (Free (SendRev b2 next)) =
        runFreeT $ composeRevBiased (f1 (Rev b2)) next

    -- Left sends to waiting right:
    go (Free (SendFwd a2 next)) a2q b2q (Free (Wait f2)) =
        runFreeT $ comp next a2q b2q (f2 (Fwd a2))

    -- Simultaneously send to each other! Here we have to buffer values:
    go (Free (SendFwd a2 next1)) a2q b2q (Free (SendRev b2 next2)) =
        runFreeT $ comp next1 (a2q S.|> a2) (b2q S.|> b2) next2


    -- Left can just freely send out:
    go (Free (SendRev b3 next)) a2q b2q freeF2 =
        runFreeT $ sendRev b3 >> comp next a2q b2q (wrapFreeF freeF2)

    -- Right can just freely send out:
    go freeF1 a2q b2q (Free (SendFwd a3 next)) =
        runFreeT $ sendFwd a3 >> comp (wrapFreeF freeF1) a2q b2q next

    -- Terminate:
    go (Pure _) _a2q _b2q _ = return $ Pure ()
    go _ _a2q _b2q (Pure _) = return $ Pure ()


(<<->) :: (Monad m) =>
    Protocol a1 a2 b3 b2 m () -> Protocol a2 a3 b2 b1 m () -> Protocol a1 a3 b3 b1 m ()
(<<->) = composeRevBiased


send :: (Monad m) => Directed a' b' -> Protocol a a' b' b m ()
send (Fwd a') = sendFwd a'
send (Rev b') = sendRev b'

waitThen ::
    (Monad m) =>
    (a -> Protocol a a' b' b m r) ->
    (b -> Protocol a a' b' b m r) ->
    Protocol a a' b' b m r
waitThen onFwd onRev = do
    d <- wait
    case d of
      Fwd a -> onFwd a
      Rev b -> onRev b

waitThenFwdOnly
  :: Monad m
  => (a -> Protocol a a' b' Void m r)
  -> Protocol a a' b' Void m r
waitThenFwdOnly f = waitThen f (const $ error "Message from the void right")

waitThenRevOnly
  :: Monad m
  => (b -> Protocol Void a' b' b m r)
  -> Protocol Void a' b' b m r
waitThenRevOnly f = waitThen (const $ error "Message from the void left") f

mapProtocol :: (Monad m) => (a -> a') -> (b -> b') -> Protocol a a' b' b m ()
mapProtocol f g = forever $ waitThen (sendFwd . f) (sendRev . g)

idProtocol :: Monad m => Protocol a a b b m ()
idProtocol = forever $ waitThen sendFwd sendRev

waitEitherForever :: IO a -> IO b -> (Either a b -> IO r) -> IO ()
waitEitherForever ioa iob f = do
    concurrently_
        (forever $ ioa >>= f . Left)
        (forever $ iob >>= f . Right)

-- | Wrap a protocol by replacing all protocol actions with corresponding
-- | effects.
hostProtocol :: (MonadIO m) =>
    IO a -> (a' -> m ()) ->
    (b' -> m ()) -> IO b ->
    Protocol a a' b' b m r ->
    Protocol Void Void Void Void m r
hostProtocol ioa ona' onb' iob protocol = FreeT $ do
    mv <- liftIO $ newEmptyMVar
    as <- liftIO $ async $ waitEitherForever ioa iob (putMVar mv)
    r <- go mv protocol
    liftIO $ cancel as
    return r
  where
    go mv proto =
      do
        t <- runFreeT proto
        case t of
          Pure r -> return $ Pure r
          Free (Wait f) -> liftIO (takeMVar mv) >>= go mv . f . either Fwd Rev
          Free (SendFwd a' next) -> ona' a' >> go mv next
          Free (SendRev b' next) -> onb' b' >> go mv next

runEffect :: (Monad m) => Protocol Void Void Void Void m r -> m r
runEffect proto = runFreeT proto >>= f
  where
    f (Pure r) = return r
    f (Free (Wait _)) = error "Waiting for a Void value!"
    f (Free (SendFwd _ _)) = error "SendFwd a Void value - how?!"
    f (Free (SendRev _ _)) = error "SendRev a Void value - how?!"

runProtocolIO :: (MonadIO m) =>
    IO a -> (a' -> m ()) ->
    (b' -> m ()) -> IO b ->
    Protocol a a' b' b m r -> m r
runProtocolIO ioa ona' onb' iob proto =
  runEffect $ hostProtocol ioa ona' onb' iob proto


-- This is some pretty neat stuff to make pattern matching on Seq work like for
-- lists. See:
-- https://www.reddit.com/r/haskell/comments/3wo40t/pattern_synonyms_in_ghc_80/
pattern EmptySeq :: S.Seq a
pattern EmptySeq <- (S.viewl -> S.EmptyL) where
    EmptySeq = S.empty

pattern (:<) :: a -> S.Seq a -> S.Seq a
pattern x :< xs <- (S.viewl -> x S.:< xs) where
    (:<) = (S.<|)

pattern (:>) :: S.Seq a -> a -> S.Seq a
pattern xs :> x <- (S.viewr -> xs S.:> x) where
    (:>) = (S.|>)

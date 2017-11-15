module TestProtocol where

import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual)

import Control.Concurrent.MVar
import Control.Monad (forever, replicateM)
import Control.Monad.State
import Control.Monad.Trans (lift)
import Control.Monad.Identity (runIdentity)
import qualified Data.Set as Set

import Clapi.Protocol


tests = [
    testCase "Protocol basic" testProtocolBasic,
    testCase "runProtocolIO" testRunProtocolIO,
    testCase "Protocol colliison" testCollision
    ]


testProtocolBasic =
    assertEqual "forwarded result" ((), "cba") result
  where
    result = runState (runEffect $ source <<-> cat <<-> sink) []
    source = (mapM_ sendFwd $ fmap Just "abc") >> sendFwd Nothing
    sink = forever $ wait >>= \(Fwd a) -> (lift $ modify (a:))
    cat = waitThen (maybe (return ()) (\a -> sendFwd a >> cat)) undefined

testRunProtocolIO = do
    am <- newMVar 'a'
    a'm <- newEmptyMVar
    bm <- newMVar 'b'
    b'm <- newEmptyMVar
    r <- runProtocolIO
        (takeMVar am) (putMVar a'm)
        (putMVar b'm) (takeMVar bm)
        blk
    a <- takeMVar a'm
    b <- takeMVar b'm
    assertEqual "a" a 'a'
    assertEqual "b" b 'b'
    assertEqual "r" r "done!"
  where
    blk = (replicateM 2 $ wait >>= send) >> return "done!"

testCollision =
  do
    assertEqual "smashed" (Set.fromList [lyricFwd, lyricRev]) smashed
  where
    smashed = execState (runEffect $ fireFwd <<-> fireRev) mempty
    lyricFwd = "always going forward"
    lyricRev = "we can't find reverse"
    fireFwd = do
      sendFwd lyricFwd
      waitThen undefined (\x -> lift . modify $ Set.insert x)
    fireRev = do
      sendRev lyricRev
      waitThen (\x -> lift . modify $ Set.insert x) undefined

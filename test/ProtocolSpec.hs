module ProtocolSpec where

import Test.Hspec

import Control.Concurrent.MVar
import Control.Monad (forever, replicateM)
import Control.Monad.State
import Control.Monad.Trans (lift)
import Control.Monad.Identity (runIdentity)
import qualified Data.Set as Set

import Clapi.Protocol

spec :: Spec
spec = do
    it "Returns result" $ basicResult `shouldBe` ((), "cba")
    it "runProtocolIO" $ rpioResult `shouldReturn` ('a', 'b', "done!")
    it "Smashed together" $ smashed `shouldBe` (Set.fromList [lyricFwd, lyricRev])
  where
    basicResult = runState (runEffect $ source <<-> cat <<-> sink) []
    source = (mapM_ sendFwd $ fmap Just ("abc" :: String)) >> sendFwd Nothing
    sink = forever $ wait >>= \(Fwd a) -> (lift $ modify (a:))
    cat = waitThenFwdOnly (maybe (return ()) (\a -> sendFwd a >> cat))
    blk = (replicateM 2 $ wait >>= send) >> return "done!"
    rpioResult = do
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
        return (a, b, r)
    smashed = execState (runEffect $ fireFwd <<-> fireRev) mempty
    lyricFwd = "always going forward"
    lyricRev = "we can't find reverse"
    fireFwd = do
      sendFwd lyricFwd
      waitThen undefined (\x -> lift . modify $ Set.insert x)
    fireRev = do
      sendRev lyricRev
      waitThen (\x -> lift . modify $ Set.insert x) undefined

module Clapi.ClientInfoCollector where
import Clapi.TimeDiffProtocol (TimeDelta)
import qualified Data.Map as Map
import Control.Concurrent.MVar

data ClientInfo = ClientInfo {ciTimeDelta :: TimeDelta}

type CollectedInfoSnapshot i = Map.Map i ClientInfo
type CollectedClientInfo i = MVar (CollectedInfoSnapshot i)

collectedClientInfo :: Ord i => IO (CollectedClientInfo i)
collectedClientInfo = newMVar mempty

clientTimeDelta :: Ord i => CollectedClientInfo i -> i -> TimeDelta -> IO ()
clientTimeDelta cci i td = modifyMVar_ cci (return . Map.insert i (ClientInfo td))

dropClientInfo :: Ord i => CollectedClientInfo i -> i -> IO ()
dropClientInfo cci i = modifyMVar_ cci (return . Map.delete i)

getCollectedInfo :: CollectedClientInfo i -> IO (CollectedInfoSnapshot i)
getCollectedInfo = readMVar

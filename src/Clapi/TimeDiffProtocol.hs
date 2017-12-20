module Clapi.TimeDiffProtocol (TimeDelta, timeDiffProto, getDelta, tdZero, tdClVal) where
import Control.Monad (forever)
import Control.Monad.Trans (lift)
import Data.Word (Word32)
import System.Clock (Clock(Monotonic), TimeSpec, getTime, toNanoSecs)

import Clapi.Types (Time(..), TimeStamped(..), ClapiValue(ClFloat))
import Clapi.PerClientProto (ClientEvent(..), ServerEvent)
import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)

timeToFloat :: Time -> Float
timeToFloat (Time s f) = fromRational $ toRational s + (toRational f / toRational (maxBound :: Word32))

getMonotonicTimeFloat :: IO Float
getMonotonicTimeFloat = (\ts -> fromRational $ (toRational $ toNanoSecs ts) / 10.0^9) <$> getTime Monotonic

newtype TimeDelta = TimeDelta Float

tdZero :: TimeDelta
tdZero = TimeDelta 0.0

getDelta :: Time -> IO TimeDelta
getDelta theirTime = (\now -> TimeDelta $ now - (timeToFloat theirTime)) <$> getMonotonicTimeFloat

tdClVal :: TimeDelta -> ClapiValue
tdClVal (TimeDelta f) = ClFloat f

timeDiffProto ::
    Monad m =>
    (m Float) ->
    (i -> TimeDelta -> m ()) ->
    (i -> m ()) ->
    Protocol
        (ClientEvent i (TimeStamped a))
        (ClientEvent i a)
        (ServerEvent i b)
        (ServerEvent i b)
        m ()
timeDiffProto getTime storeDelta dropDelta = forever $ waitThen fwd sendRev
  where
    fwd (ClientData i (TimeStamped (theirTime, a))) = do
        let theirTimeF = timeToFloat theirTime
        ourTimeF <- lift getTime
        lift $ storeDelta i $ TimeDelta $ ourTimeF - theirTimeF
        sendFwd $ ClientData i a
    fwd (ClientConnect i)  = sendFwd $ ClientConnect i
    fwd (ClientDisconnect i) = do
        lift $ dropDelta i
        sendFwd $ ClientDisconnect i

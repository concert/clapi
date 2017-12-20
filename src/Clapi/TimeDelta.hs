{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Clapi.TimeDelta (TimeDelta, getDelta, tdZero) where
import Data.Word (Word32)
import System.Clock (Clock(Monotonic), TimeSpec, getTime, toNanoSecs)

import Clapi.Types (Time(..), TimeStamped(..), ClapiValue(ClFloat), Clapiable(..))

timeToFloat :: Time -> Float
timeToFloat (Time s f) = fromRational $ toRational s + (toRational f / toRational (maxBound :: Word32))

getMonotonicTimeFloat :: IO Float
getMonotonicTimeFloat = (\ts -> fromRational $ (toRational $ toNanoSecs ts) / 10.0^9) <$> getTime Monotonic

newtype TimeDelta = TimeDelta Float deriving Num

tdZero :: TimeDelta
tdZero = TimeDelta 0.0

getDelta :: Time -> IO TimeDelta
getDelta theirTime = (\now -> TimeDelta $ now - (timeToFloat theirTime)) <$> getMonotonicTimeFloat

instance Clapiable TimeDelta where
    toClapiValue (TimeDelta f) = ClFloat f
    fromClapiValue (ClFloat f) = return $ TimeDelta f
    fromClapiValue _ = fail "bad type"

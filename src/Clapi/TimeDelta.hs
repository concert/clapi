{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Clapi.TimeDelta where -- (TimeDelta, getDelta, tdZero) where

import Data.Word (Word32)
import System.Clock (Clock(Monotonic), getTime, toNanoSecs)

import Clapi.Types (Time(..))
import Clapi.Serialisation.Base (Encodable)

timeToFloat :: Time -> Float
timeToFloat (Time s f) =
  fromRational $ toRational s + (toRational f / toRational (maxBound :: Word32))

getMonotonicTimeFloat :: IO Float
getMonotonicTimeFloat = getTime Monotonic >>=
    return . fromRational . (/10.0^(9 :: Int)) . toRational . toNanoSecs

newtype TimeDelta = TimeDelta {unTimeDelta :: Float} deriving (Num, Encodable)

tdZero :: TimeDelta
tdZero = TimeDelta 0.0

getDelta :: Time -> IO TimeDelta
getDelta theirTime = getMonotonicTimeFloat >>=
  return . TimeDelta . subtract (timeToFloat theirTime)

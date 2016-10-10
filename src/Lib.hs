{-# LANGUAGE DeriveDataTypeable #-}
module Lib
    (
        someFunc,
    ) where

import Path (BasePath(..), Method(..))
import Types (
    ClapiMessage(..), ClapiPath, ClapiMethod(..), ClapiValue(..),
    ClapiMessageTag, ClapiBundle
    )
import Blaze.ByteString.Builder (toByteString)
import Data.Data (Typeable, Data, dataTypeOf, dataTypeConstrs, Constr, toConstr)
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map

someFunc :: IO ()
someFunc = putStrLn "Hello World"

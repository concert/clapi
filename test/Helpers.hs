{-# LANGUAGE FlexibleInstances #-}
module Helpers where

import Control.Monad.Fail (MonadFail, fail)
import Data.Either (either)
import Data.Either.Combinators (fromLeft)
import Data.List (isInfixOf)
import Data.Maybe (isJust, fromJust)
import System.Timeout
import Test.HUnit (Assertion, assertBool, assertFailure)


seconds = truncate . (* 1e6)

-- FIXME: could try to update to use MonadExcept?
assertFailed :: Show a => String -> Either String a -> Assertion
assertFailed s = either
    (const $ return ())
    (\x -> assertFailure $ s ++ " did not fail, instead was " ++ show x)


assertFailedSubstr :: Show b => String -> String -> Either String b -> Assertion
assertFailedSubstr msg substr e =
  do
    assertFailed (msg ++ "did not fail") e
    assertBool (
        msg ++ "failed with \"" ++ "x" ++ "\",\nnot\"" ++ substr ++
        "\"") $ substr `isInfixOf` actualMsg
  where
    actualMsg = fromLeft "" e

timeLimit :: Double -> IO a -> IO a
timeLimit secs action = (timeout (seconds secs) action) >>= \r ->
    assertBool "timed out" (isJust r) >> return (fromJust r)

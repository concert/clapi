{-# LANGUAGE FlexibleInstances #-}
module Helpers where

import Control.Monad.Fail (MonadFail, fail)
import Data.Either (either)
import Data.List (isInfixOf)
import Data.Either.Combinators (fromLeft)
import Test.HUnit (Assertion, assertBool)


instance MonadFail (Either String) where
    fail = Left

-- FIXME: could try to update to use MonadExcept?
assertFailed :: String -> Either String a -> Assertion
assertFailed s =
     assertBool (s ++ " did not fail") . either (const True) (const False)


assertFailedSubstr :: String -> String -> Either String b -> Assertion
assertFailedSubstr msg substr e =
  do
    assertFailed (msg ++ "did not fail") e
    assertBool (
        msg ++ "failed with \"" ++ "x" ++ "\",\nnot\"" ++ substr ++
        "\"") $ substr `isInfixOf` actualMsg
  where
    actualMsg = fromLeft "" e

module Util where

import Test.HUnit (Assertion, assertBool)


assertFailed :: String -> Either a b -> Assertion
assertFailed s either = assertBool s (didFail either)
  where
    didFail (Left _) = True
    didFail (Right _) = False

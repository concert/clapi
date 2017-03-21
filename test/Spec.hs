module Main (
    main
) where

import Test.Framework (defaultMain, Test)

import qualified TestPipeline (tests)
import qualified TestServer (tests)
import qualified TestTypes (tests)
import qualified TestTree (tests)
import qualified TestValuespace (tests)
import qualified TestValidator (tests)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = mconcat [
    TestPipeline.tests,
    TestServer.tests,
    TestTypes.tests,
    TestValuespace.tests,
    TestValidator.tests,
    TestTree.tests
    ]

module Main (
    main
) where

import Test.Framework (defaultMain, Test)

import qualified Data.Map.TestMos (tests)
import qualified TestProtocol (tests)
import qualified TestNamespaceTracker (tests)
import qualified TestServer (tests)
import qualified TestTypes (tests)
import qualified TestTree (tests)
import qualified TestUtil (tests)
import qualified TestValuespace (tests)
import qualified TestValidator (tests)
import qualified TestEncodingProtocol (tests)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = mconcat [
    TestUtil.tests,
    Data.Map.TestMos.tests,
    TestProtocol.tests,
    TestNamespaceTracker.tests,
    TestServer.tests,
    TestTypes.tests,
    TestValuespace.tests,
    TestValidator.tests,
    TestTree.tests,
    TestEncodingProtocol.tests
    ]

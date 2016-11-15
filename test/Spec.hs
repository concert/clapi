module Main (
    main
) where

import Test.Framework (defaultMain, Test)

import qualified TestTypes (tests)
import qualified TestTree (tests)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = mconcat [
    TestTypes.tests,
    TestTree.tests
    ]

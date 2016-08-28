module Main (
    main
) where

import Test.Framework (defaultMain, Test)
import Test.Framework.Providers.HUnit

import TestPath

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [
        testCase "Parse BasePath" testParseBasePath,
        testCase "Fail parse BasePath" testFailParseBasePath
    ]

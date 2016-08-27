module Main (
    main
) where

import Test.Framework (defaultMain, Test)
import Test.Framework.Providers.HUnit

import Tests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [
        testCase "Display BasePath" testDisplayBasePath,
        testCase "Parse BasePath" testParseBasePath
    ]

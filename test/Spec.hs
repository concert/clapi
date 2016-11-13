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
        testCase "roundtrip ClapiValue" testClapiValueConversionRoundTrip,
        testCase "roundtrip message" testBinarySerialisationRoundTrip,
        testCase "string length" testEncodeTooLongString,
        testCase "test treeGet" testTreeGet,
        testCase "test treeAdd" testTreeAdd,
        testCase "test treeSet" testTreeSet,
        testCase "test treeDelete" testTreeDelete
    ]

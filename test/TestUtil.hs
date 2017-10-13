module TestUtil where

import Helpers (assertFailed)
import Test.HUnit (assertEqual)
import Test.Framework.Providers.HUnit (testCase)

import qualified Data.Set as Set

import Clapi.Types ()
import Clapi.Util (duplicates, zipLongest, strictZip)

tests = [
    testCase "duplicates" testDuplicates,
    testCase "zipLongest" testZipLongest,
    testCase "strictZip" testStrictZip
    ]


testDuplicates =
  do
    assertEqual "empty" mempty $ duplicates ([] :: [Char])
    assertEqual "unique" mempty $ duplicates ['a', 'b', 'c', 'd']
    assertEqual "duplicates" dups $ duplicates as
  where
    as = ['a', 'b', 'a', 'c', 'd', 'd', 'e', 'd']
    dups = Set.fromList ['a', 'd']


testZipLongest =
  do
    assertEqual "a longer" [("a", "x"), ("b", "")] $ zipLongest ["a", "b"] ["x"]
    assertEqual "b longer" [(Just "a", Just "x"), (Nothing, Just "y")] $
        zipLongest [Just "a"] (Just <$> ["x", "y"])
    assertEqual "same" [("a", "b")] $ zipLongest ["a"] ["b"]

testStrictZip =
  do
    assertFailed "strictZip" $ strictZip ["a", "b"] ["x"]
    assertFailed "strictZip" $ strictZip ["a"] ["x", "y"]
    assertEqual "same" (Just [("a", "x")]) $ strictZip ["a"] ["x"]

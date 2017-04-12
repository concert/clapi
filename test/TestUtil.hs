module TestUtil where

import Test.HUnit (assertEqual)
import Test.Framework.Providers.HUnit (testCase)

import qualified Data.Set as Set

import Util (duplicates)

tests = [
    testCase "duplicates" testDuplicates
    ]


testDuplicates =
  do
    assertEqual "empty" mempty $ duplicates ([] :: [Char])
    assertEqual "unique" mempty $ duplicates ['a', 'b', 'c', 'd']
    assertEqual "duplicates" dups $ duplicates as
  where
    as = ['a', 'b', 'a', 'c', 'd', 'd', 'e', 'd']
    dups = Set.fromList ['a', 'd']

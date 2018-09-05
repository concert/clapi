{-# OPTIONS_GHC -Wno-type-defaults #-}

module Data.Map.DependenciesSpec where

import Test.Hspec

import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.Map.Dependencies (Dependencies)
import qualified Data.Map.Dependencies as Dependencies

spec :: Spec
spec =
  describe "Dependencies" $ do
    it "Singularly works" $
      let
        ds0 = mempty
        ds1 = Dependencies.insert 'a' 1 ds0
        ds2 = Dependencies.insert 'b' 1 ds1
        ds3 = Dependencies.insert 'a' 2 ds2
        ds4 = Dependencies.delDependency 'b' ds3
        ds5 = Dependencies.delDependency 'a' ds4
      in do
        assertDep 'a' 1 ds1
        assertRevDeps 1 "a" ds1
        assertDep 'b' 1 ds2
        assertRevDeps 1 "ab" ds2
        assertDep 'a' 2 ds3
        assertRevDeps 1 "b" ds3
        assertRevDeps 2 "a" ds3
        assertNoDep 'b' ds4
        assertNoRevDeps 1 ds4
        ds5 `shouldBe` ds0
    it "Multiply works" $
      let
        ds0 = mempty
        ds1 = Dependencies.setDependencies (Map.fromList [('a', 1), ('b', 1), ('c', 2)]) ds0
        ds2 = Dependencies.delDependencies ['a', 'c'] ds1
        ds3 = Dependencies.delDependencies ['b'] ds2
      in do
        assertDep 'a' 1 ds1
        assertDep 'b' 1 ds1
        assertDep 'c' 2 ds1
        assertRevDeps 1 "ab" ds1
        assertRevDeps 2 "c" ds1
        assertNoDep 'a' ds2
        assertNoDep 'c' ds2
        assertDep 'b' 1 ds2
        assertRevDeps 1  "b" ds2
        assertNoRevDeps 2 ds2
        ds3 `shouldBe` ds0
    it "filterWithKey" $ Dependencies.filterWithKey
        (\k a -> k == 3 || a == "hi")
        (Dependencies.fromList [(1, "hi"), (2, "ho"), (3, "silver")])
        `shouldBe` Dependencies.fromList [(1, "hi"), (3, "silver")]

assertDep :: Char -> Int -> Dependencies Char Int -> Expectation
assertDep k a ds = Dependencies.lookup k ds `shouldBe` Just a

assertNoDep :: Char -> Dependencies Char Int -> Expectation
assertNoDep k ds = Dependencies.lookup k ds `shouldBe` Nothing

assertRevDeps :: Int -> [Char] -> Dependencies Char Int -> Expectation
assertRevDeps a ks ds =
    Dependencies.lookupRev a ds `shouldBe` Set.fromList ks

assertNoRevDeps :: Int -> Dependencies Char Int -> Expectation
assertNoRevDeps a ds = Dependencies.lookupRev a ds `shouldBe` mempty

module Data.Map.TestMos where

import Test.HUnit (assertEqual)
import Test.Framework.Providers.HUnit (testCase)

import qualified Data.Set as Set

import qualified Data.Map.Mos as Mos

tests = [
    testCase "Mos doesn't leak" testNoLeak,
    testCase "dependency map" testDependency]

testNoLeak = assertEqual "dirty round trip" mempty $
    Mos.delete 3 'a' $ Mos.insert 3 'a' mempty

testDependency =
  let
    ds0 = (mempty, mempty)
    ds1 = Mos.setDependency 'a' 1 ds0
    ds2 = Mos.setDependency 'b' 1 ds1
    ds3 = Mos.setDependency 'a' 2 ds2
    ds4 = Mos.delDependency 'b' ds3
    ds5 = Mos.delDependency 'a' ds4
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
    assertEqual "empty" ds0 ds5
  where
    assertDep k a ds = assertEqual "dep" (Just a) (Mos.getDependency k ds)
    assertNoDep k ds = assertEqual "no dep" Nothing (Mos.getDependency k ds)
    assertRevDeps a ks ds =
        assertEqual "revDeps" (Just $ Set.fromList ks) (Mos.getDependants a ds)
    assertNoRevDeps a ds =
        assertEqual "no devDep" Nothing (Mos.getDependants a ds)

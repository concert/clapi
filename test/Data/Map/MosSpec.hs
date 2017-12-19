module Data.Map.MosSpec where

import Test.Hspec
import Test.HUnit (assertEqual)

import qualified Data.Set as Set

import qualified Data.Map as Map
import qualified Data.Map.Mos as Mos

spec :: Spec
spec = do
    it "Doesn't leak" $ (Mos.delete 3 'a' $ Mos.insert 3 'a' mempty) `shouldBe` mempty
    it "Singularly works" $
      let
        ds0 = mempty
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
        ds5 `shouldBe` ds0
    it "Multiply works" $
      let
        ds0 = mempty
        ds1 = Mos.setDependencies (Map.fromList [('a', 1), ('b', 1), ('c', 2)]) ds0
        ds2 = Mos.delDependencies "ac" ds1
        ds3 = Mos.delDependencies "b" ds2
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

assertDep k a ds = Mos.getDependency k ds `shouldBe` Just a
assertNoDep k ds = Mos.getDependency k ds `shouldBe` Nothing
assertRevDeps a ks ds =
    Mos.getDependants a ds `shouldBe` Just (Set.fromList ks)
assertNoRevDeps a ds = Mos.getDependants a ds `shouldBe` Nothing

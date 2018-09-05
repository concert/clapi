{-# OPTIONS_GHC -Wno-type-defaults #-}

module Data.Map.MosSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Map.Mos as Mos

spec :: Spec
spec =
  describe "Mos" $ do
    it "Doesn't leak" $
      let input = Mos.fromList [(1, 'a'), (2, 'a'), (2, 'b')] in do
        Mos.delete 1 'a' input `shouldBe` Mos.fromList [(2, 'a'), (2, 'b')]
        Mos.remove 'a' input `shouldBe` Mos.singleton 2 'b'
        Mos.difference input input `shouldBe` mempty
        Mos.partition (== 'a') input `shouldBe`
          (Mos.fromList [(1, 'a'), (2, 'a')], Mos.singleton 2 'b')
        Mos.singletonSet 'a' (mempty :: Set Int) `shouldBe` mempty
    it "should stay correct under inversion" $ property $
      \(input :: [(Char, Int)]) -> let mos = Mos.fromList input in
        mos == Mos.invert (Mos.invert mos)
    it "should roundtrip via set" $ property $
      \(input :: [(Char, Int)]) -> let mos = Mos.fromList input in
        mos == Mos.fromFoldable (Mos.toSet mos)
    it "can be produced from a Map" $ property $
      \(input :: Map Char Int) -> let mos = Mos.invertMap input in
        Mos.fromList (Map.toList input) == Mos.invert mos
    it "intersects with another" $
      let
        m1 = Mos.fromList [(1, 'a'), (1, 'b'), (2, 'a'), (3, 'a')]
        m2 = Mos.fromList [(1, 'a'), (2, 'a'), (2, 'b'), (4, 'a')]
        me = Mos.fromList [(1, 'a'), (2, 'a')]
      in
        Mos.intersection m1 m2 `shouldBe` me

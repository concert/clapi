module UtilSpec where

import Test.Hspec

import Prelude hiding (map)
import qualified Data.Set as Set

import Clapi.Util
  (Mappable(..), duplicates, strictZip, filterMaybe, justs, rights, lefts)

spec :: Spec
spec = do
    describe "duplicates" $ do
        it "Empty when empty" $ duplicates ([] :: [Char]) `shouldBe` mempty
        it "Empty when unique" $ duplicates ['a', 'b', 'c', 'd'] `shouldBe` mempty
        it "Finds dups" $
            duplicates ['a', 'b', 'a', 'c', 'd', 'd', 'e', 'd'] `shouldBe`
            Set.fromList ['a', 'd']

    describe "strictZip" $ do
        it "fails when b shorter" $
          strictZip ["a", "b"] ["x"] `shouldBe` Left (2, 1)
        it "fails when a shorter" $
          strictZip ["a"] ["x", "y"] `shouldBe` Left (1, 2)
        it "zips when same" $
          strictZip ["a"] ["x"] `shouldBe` Right [("a", "x")]

    describe "Mappable.map" $ do
      it "can map over a regular functor" $
        map succ "abc" `shouldBe` "bcd"
      it "can map over `Set`s" $
        map succ (Set.fromList "abc") `shouldBe` Set.fromList "bcd"

    describe "filterMaybe" $
      it "Transforms elements and drops `Nothing`s" $
        filterMaybe
            (\x -> if x == 'b' then Nothing else Just $ succ $ succ x)
            (Set.fromList "abc")
        `shouldBe` Set.fromList "ce"

    describe "justs" $
      it "Drops `Nothing`s" $
        justs [Just 'a', Nothing, Just 'b'] `shouldBe` ['a', 'b']

    describe "lefts" $
      it "Drops `Right`s" $
        lefts [Left 'a', Right 'b', Left 'c'] `shouldBe` ['a', 'c']

    describe "rights" $
      it "Drops `Left`s" $
        rights [Left 'a', Right 'b', Left 'c'] `shouldBe` ['b']

module UtilSpec where

import Test.Hspec

import qualified Data.Set as Set

import Clapi.Util (duplicates, zipLongest, strictZip)

spec :: Spec
spec = do
    describe "duplicates" $ do
        it "Empty when empty" $ duplicates ([] :: [Char]) `shouldBe` mempty
        it "Empty when unique" $ duplicates ['a', 'b', 'c', 'd'] `shouldBe` mempty
        it "Finds dups" $
            duplicates ['a', 'b', 'a', 'c', 'd', 'd', 'e', 'd'] `shouldBe`
            Set.fromList ['a', 'd']
    describe "zipLongest" $ do
        it "a longer" $ zipLongest ["a", "b"] ["x"] `shouldBe` [("a", "x"), ("b", "")]
        it "b longer" $ zipLongest [Just "a"] (Just <$> ["x", "y"]) `shouldBe`
            [(Just "a", Just "x"), (Nothing, Just "y")]
        it "same length" $ zipLongest ["a"] ["b"] `shouldBe` [("a", "b")]
    describe "strictZip" $ do
        it "fails when b shorter" $ strictZip ["a", "b"] ["x"] `shouldBe` Nothing
        it "fails when a shorter" $ strictZip ["a"] ["x", "y"] `shouldBe` Nothing
        it "zips when same" $ strictZip ["a"] ["x"] `shouldBe` Just [("a", "x")]

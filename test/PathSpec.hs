{-# LANGUAGE
    OverloadedStrings
#-}

module PathSpec where

import Test.Hspec
import Test.QuickCheck (property)

import Data.Text (Text)
import Data.Either (isLeft)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Clapi.TH (pathq, nameq)
import Clapi.Types (CanFail)
import Clapi.Types.Path
  ( Path'(..), Path, pattern Root, pattern (:/), pattern (:</), fromText, toText
  , isChildOf, unName, nameP, prefixes, prefixesMap, isStrictChildOfAny)

import Arbitrary ()


spec :: Spec
spec = do
    describe "Name Semigroup instance" $ it "Joins names as expected" $
        [nameq|yo|] <> [nameq|ho|] <> [nameq|ahoy|] `shouldBe` [nameq|yo_ho_ahoy|]
    describe "Quasiquoter" $ it "Produces expected path" $
      [pathq|/oi/mate|] `shouldBe` Path' [[nameq|oi|], [nameq|mate|]]
    describe ":/" $ do
        it "Splits the end off" $ let (p :/ s) = [pathq|/a/b/c|] in do
            p `shouldBe` [pathq|/a/b|]
            s `shouldBe` [nameq|c|]
        it "Appends a name" $ [pathq|/a/b|] :/ [nameq|c|] `shouldBe` [pathq|/a/b/c|]
    describe ":</" $ do
        it "Splits the start off" $ let (s :</ p) = [pathq|/a/b/c|] in do
            s `shouldBe` [nameq|a|]
            p `shouldBe` [pathq|/b/c|]
        it "Prepends a name" $ [nameq|a|] :</ [pathq|/b/c|] `shouldBe` [pathq|/a/b/c|]
    describe "From text" $ do
        it "Root" $ "/" `shouldBeGoodPath` Root
        it "Single namement" $ "/foo" `shouldBeGoodPath` (Root :/ [nameq|foo|])
        it "Multi namement" $ "/foo/bar" `shouldBeGoodPath` (Root :/ [nameq|foo|] :/ [nameq|bar|])
        -- Consider, this should be legal?
        -- it "Trailing /" $ "/foo/bar/" `shouldBeGoodPath` (Root :/ [nameq|foo|] :/ [nameq|bar|])
        it "Fails with no leading /" $ shouldBeBadPath "foo/bar"
        it "No leading /" $ shouldBeBadPath "foo/bar"
    describe "Round trip" $ do
        it "Empty path" $ rt "/"
        it "Top level path" $ rt "/fdoo"
        it "Nested path" $ rt "/boo_1/chew_2"
    describe "isChildOf" $ do
        it "Child -> True" $ [pathq|/a|] `shouldSatisfy` isChildOf [pathq|/a/b/c/d|]
        it "Non-child -> False" $ [pathq|/a|] `shouldNotSatisfy` isChildOf [pathq|/b/a/g|]
        it "Parent -> False" $ [pathq|/a/b|] `shouldNotSatisfy` isChildOf [pathq|/a|]
        it "Same path -> True" $ [pathq|/a/b|] `shouldSatisfy` isChildOf [pathq|/a/b|]
    describe "prefixesMap" $ do
        it "should always produce a valid map" $ property $
          \(m :: Map Path Int) -> Map.valid $ prefixesMap m
        it "should keep values with their keys" $ property $
          \(m :: Map Path Int) -> let result = prefixesMap m in
            result == m `Map.intersection` result
    describe "prefixes" $ do
        it "should work on a simple example" $
          let
            paths = Set.fromList
              [ [pathq|/a/b|]
              , [pathq|/a/c|]
              , [pathq|/a/b/d|]
              , [pathq|/a/b/e|]
              , [pathq|/f|]
              , [pathq|/f/g/h/i|]
              , [pathq|/j/k/l/m|]
              ]
          in
            prefixes paths `shouldBe` Set.fromList
              [ [pathq|/a/b|]
              , [pathq|/a/c|]
              , [pathq|/f|]
              , [pathq|/j/k/l/m|]
              ]
        it "should produce a set containing no child paths" $ property $
          \(s :: Set Path) -> let result = prefixes s in
            all (not . flip isStrictChildOfAny result) result
  where
    shouldBeGoodPath t p = (fromText segP t :: CanFail Path) `shouldBe` Right p
    shouldBeBadPath t = (fromText segP t :: CanFail Path) `shouldSatisfy` isLeft
    rt p = (toText unSeg <$> fromText segP p :: CanFail Text) `shouldBe` Right p

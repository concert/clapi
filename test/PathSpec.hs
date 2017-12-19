{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module PathSpec where

import Test.Hspec
import Test.QuickCheck (property)

import Data.Either (isLeft)

import Clapi.Path (Path(..), Name, root, up)
import Clapi.PathQ (pathq)
import Path.Parsing (toString, fromString)
import Clapi.Types (CanFail)


spec :: Spec
spec = do
    describe "Quasiquoter" $ it "Produces expected path" $ [pathq|/oi/mate|] `shouldBe` Path ["oi", "mate"]
    describe "Up" $ do
        it "Gets the parent" $ up (Path ["a", "b"]) `shouldBe` Path ["a"]
        it "Does nothing at the root" $ up root `shouldBe` root
    describe "From string" $ do
        it "Root" $ "/" `shouldBeGoodPath` []
        it "Single segment" $ "/foo" `shouldBeGoodPath` ["foo"]
        it "Multi segment" $ "/foo/bar" `shouldBeGoodPath` ["foo", "bar"]
        it "Trailing /" $ "/foo/bar/" `shouldBeGoodPath` ["foo", "bar"]
        it "Fails with no leading /" $ shouldBeBadPath "foo/bar"
        it "No leading /" $ shouldBeBadPath "foo/bar"
    describe "Round trip" $ do
        it "Empty path" $ rt root
        it "Top level path" $ rt $ Path ["fdoo"]
        it "Nested path" $ rt $ Path ["boo_1", "chew_2"]
  where
    shouldBeGoodPath str segs = (fromString str :: CanFail Path) `shouldBe` (Right $ Path segs)
    shouldBeBadPath str = (fromString str :: CanFail Path) `shouldSatisfy` isLeft
    rt p = (fromString $ toString p :: CanFail Path) `shouldBe` Right p

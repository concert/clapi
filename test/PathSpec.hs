{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
module PathSpec where

import Test.Hspec

import Data.Text (Text)
import Data.Either (isLeft)

import Clapi.PathQ (pathq, segq)
import Clapi.Types (CanFail)
import Clapi.Types.Path
  (Path(..), Seg, pattern Root, pattern (:/), pattern (:</), fromText, toText)


spec :: Spec
spec = do
    describe "Quasiquoter" $ it "Produces expected path" $ [pathq|/oi/mate|] `shouldBe` Path [[segq|oi|], [segq|mate|]]
    describe ":/" $ do
        it "Splits the end off" $ let (p :/ s) = [pathq|/a/b/c|] in do
            p `shouldBe` [pathq|/a/b|]
            s `shouldBe` [segq|c|]
        it "Appends a seg" $ [pathq|/a/b|] :/ [segq|c|] `shouldBe` [pathq|/a/b/c|]
    describe ":</" $ do
        it "Splits the start off" $ let (s :</ p) = [pathq|/a/b/c|] in do
            s `shouldBe` [segq|a|]
            p `shouldBe` [pathq|/b/c|]
        it "Prepends a seg" $ [segq|a|] :</ [pathq|/b/c|] `shouldBe` [pathq|/a/b/c|]
    describe "From text" $ do
        it "Root" $ "/" `shouldBeGoodPath` Root
        it "Single segment" $ "/foo" `shouldBeGoodPath` (Root :/ [segq|foo|])
        it "Multi segment" $ "/foo/bar" `shouldBeGoodPath` (Root :/ [segq|foo|] :/ [segq|bar|])
        -- Consider, this should be legal?
        -- it "Trailing /" $ "/foo/bar/" `shouldBeGoodPath` (Root :/ [segq|foo|] :/ [segq|bar|])
        it "Fails with no leading /" $ shouldBeBadPath "foo/bar"
        it "No leading /" $ shouldBeBadPath "foo/bar"
    describe "Round trip" $ do
        it "Empty path" $ rt "/"
        it "Top level path" $ rt "/fdoo"
        it "Nested path" $ rt "/boo_1/chew_2"
  where
    shouldBeGoodPath t p = (fromText t :: CanFail Path) `shouldBe` Right p
    shouldBeBadPath t = (fromText t :: CanFail Path) `shouldSatisfy` isLeft
    rt p = (toText <$> fromText p :: CanFail Text) `shouldBe` Right p

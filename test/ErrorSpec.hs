module ErrorSpec where

import Test.Hspec

import Control.Monad.State (modify)
import Data.Bifunctor (first)

import Clapi.Error


spec :: Spec
spec = do
  describe "aborts" $ do
    it "reports the first failure and terminates execution" $
      (inc >> aborts ["hello"] >> aborts ["world"] >> inc)
        `shouldFailWith` (["hello"], 1)

  describe "reports" $ do
    it "reports failures in order and continues execution" $
      (inc >> reports ["hello"] >> reports ["world"] >> inc)
        `shouldFailWith` (["hello", "world"], 2)

  describe "soften" $ do
    it "returns a result if it can" $ do
      soften (return 'a') `shouldSucceedWith` (Just 'a', 0)
      (do
        mb <- soften $ aborts ["hello"] >> return 42
        -- Checking that we couldn't return a result, so got Nothing
        incN $ maybe 12 id mb
        ) `shouldFailWith` (["hello"], 12)
    it "translates terminal failures into reported errors" $
      (soften (inc >> aborts ["hello"] >> incN 2 ) >> incN 3)
      `shouldFailWith` (["hello"], 4)

  describe "harden" $ do
    it "translates existing reported errors into terminal failures" $
      (harden (inc >> reports ["hello"] >> incN 2) >> incN 3)
      `shouldFailWith` (["hello"], 3)
    it "passes through existing terminal errors" $
      (harden (inc >> aborts ["hello"] >> incN 2) >> incN 3)
      `shouldFailWith` (["hello"], 1)

  describe "collect" $ do
    it "executes all the actions in the traversable" $
      collect (return <$> ['a', 'b']) `shouldSucceedWith` (['a', 'b'], 0)
    it "fails hard if any actions fail hard" $
      (collect
        [ inc >> return 'a'
        , aborts ["die1"]
        , inc >> return 'c'
        , reports ["die2"] >> return 'd'
        ]
       >> inc) `shouldFailWith` (["die1", "die2"], 2)
    it "only reports if any no actions fail hard" $
      (collect
        [ reports ["die1"] >> return 'a'
        , inc >> return 'b'
        , reports ["die2"] >> return 'c'
        ]
       >> inc) `shouldFailWith` (["die1", "die2"], 2)



shouldRunWith
  :: (Eq a, Show a)
  => ErrsM Int [String] a -> (Either [String] a, Int) -> Expectation
shouldRunWith m (expRes, expS) = let (res, s) = runErrsM m 0 in do
  res `shouldBe` expRes
  s `shouldBe` expS

shouldSucceedWith
  :: (Eq a, Show a) => ErrsM Int [String] a -> (a, Int) -> Expectation
shouldSucceedWith m = shouldRunWith m . first Right

shouldFailWith
  :: (Eq a, Show a)
  => ErrsM Int [String] a -> ([String], Int) -> Expectation
shouldFailWith m = shouldRunWith m . first Left

inc :: Monoid e => ErrsM Int e ()
inc = incN 1

incN :: Monoid e => Int -> ErrsM Int e ()
incN n = modify (+n)

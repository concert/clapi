module ErrorSpec where

import Test.Hspec

import Control.Monad.Except (throwError)
import Control.Monad.State (modify)
import Control.Monad.Writer (tell)
import Data.Bifunctor (first)

import Clapi.Types.Error


spec :: Spec
spec = do
  describe "throwError" $ do
    it "reports the failure and terminates execution" $
      (inc >> throwError ["hello"] >> throwError ["world"] >> inc)
      `shouldFailWith` (["hello"], 1)

  describe "tell" $ do
    it "reports failures in order and continues execution" $
      (inc >> tell ["hello"] >> tell ["world"] >> inc)
      `shouldFailWith` (["hello", "world"], 2)

  describe "soften" $ do
    it "returns a result if it can" $ do
      soften (return 'a') `shouldSucceedWith` (Just 'a', 0)
      (do
        mb <- soften $ throwError ["hello"] >> return 42
        -- Checking that we couldn't return a result, so got Nothing:
        incN $ maybe 12 id mb
        ) `shouldFailWith` (["hello"], 12)

    it "translates terminal failures into reported errors" $
      (soften (inc >> throwError ["hello"] >> incN 2) >> incN 3)
      `shouldFailWith` (["hello"], 4)

  describe "harden" $ do
    it "translates existing reported errors into terminal failures" $
      (harden (inc >> tell ["hello"] >> incN 2) >> incN 3)
      `shouldFailWith` (["hello"], 3)
    it "passes through existing terminal errors" $
      (harden (inc >> throwError ["hello"] >> incN 2) >> incN 3)
      `shouldFailWith` (["hello"], 1)
    it "does not fail if there are no errors" $
      (harden inc >> inc) `shouldSucceedWith` ((), 2)

  describe "collect" $ do
    it "executes all the actions in the traversable" $
      collect (return <$> ['a', 'b']) `shouldSucceedWith` (['a', 'b'], 0)
    it "fails hard if any actions fail hard" $
      (collect
        [ inc >> return 'a'
        , throwError ["die1"]
        , inc >> return 'b'
        , tell ["die2"] >> return 'd'
        ]
      >> inc) `shouldFailWith` (["die1", "die2"], 2)
    it "should tell errors when no actions fail hard" $
      (collect
        [ tell ["die1"] >> return 'a'
        , inc >> return 'b'
        , tell ["die2"] >> return 'c'
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
  :: (Eq a, Show a) => ErrsM Int [String] a -> ([String], Int) -> Expectation
shouldFailWith m = shouldRunWith m . first Left

inc :: Monoid e => ErrsM Int e ()
inc = incN 1

incN :: Monoid e => Int -> ErrsM Int e ()
incN n = modify (+n)

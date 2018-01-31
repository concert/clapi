{-# OPTIONS_GHC -Wall -Wno-orphans #-}

module SequenceOpsSpec (spec) where

import Clapi.Types.UniqList (mkUniqList)
import Data.Maybe (fromJust)
import Test.Hspec
import Clapi.Types ()  -- MonadFail instance (Either String)
import Clapi.Types.SequenceOps

spec :: Spec
spec = do
    it "No changes is noop" $ shouldApplyAs [] [1..4] $ Right [1..4]
    it "Basic add" $
        shouldApplyAs [(6, AddAfter $ Just 2)] [1..3] $ Right [1,2,6,3]
    it "Basic move" $
        shouldApplyAs [(3, MoveAfter $ Just 1)] [1..3] $ Right [1,3,2]
    it "Basic move to start" $
        shouldApplyAs [(3, MoveAfter Nothing)] [1..3] $ Right [3,1,2]
    it "Basic delete" $ shouldApplyAs [(2, DelElem)] [1..3] $ Right [1,3]
    it "Fails duplicate add" $ shouldApplyAs [(2, AddAfter Nothing)] [1..3] $
        Left "Cannot add element twice"
    it "Fails moving non-existant target" $
        shouldApplyAs [(3, MoveAfter Nothing)] [1..2] $
            Left "Element was not present to move: 3"
    it "Fails moving non-existant reference" $
        shouldApplyAs [(2, MoveAfter $ Just 3)] [1..2] $
            Left "Cannot add after non-existant element"
    it "Fails deleting absentee" $
        shouldApplyAs [(3, DelElem)] [1..2] $
            Left "Element not present to remove: 3"
    it "Applies multiple independant ops" $ shouldApplyAs
        [(4, AddAfter Nothing), (3, MoveAfter $ Just 1)]
        [1..3] $ Right [4,1,3,2]
    it "Applies multiple dependant ops in order" $
      let
        expected = Right [1,3,2,7,4]
        ops = [(7, AddAfter $ Just 2), (2, MoveAfter $ Just 3)]
      in do
        shouldApplyAs ops [1..4]  expected
        shouldApplyAs (reverse ops) [1..4] expected
    it "Fails when using deleted reference" $
        shouldApplyAs [(2, MoveAfter $ Just 3), (3, DelElem)] [1..3] $
            Left "Cannot add after non-existant element"
    it "Fails on cyclic input" $ shouldApplyAs
        [(1, MoveAfter $ Just 2), (2, MoveAfter $ Just 1)] [0..3] $
        Left "Unresolvable order dependencies"
  where
    shouldApplyAs
        :: [(Int, SequenceOp Int)] -> [Int] -> Either String [Int] -> Expectation
    shouldApplyAs ops start res =
        applyDigest (digest $ ReorderBundle ops) (dodgyUl start) `shouldBe` dodgyUl <$> res
      where
        dodgyUl = fromJust . mkUniqList

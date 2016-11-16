module TestTree where

import Util (assertFailed)
import Test.HUnit (assertEqual)
import Test.Framework.Providers.HUnit (testCase)

import qualified Data.Map.Strict as Map

import Types (ClapiValue(..))
import Tree (
    Tuple(..), ClapiTree(..), treeGet, treeAdd, treeSet, treeDelete,
    mapDiff, Delta(..)
    )

tests = [
    testCase "test treeGet" testTreeGet,
    testCase "test treeAdd" testTreeAdd,
    testCase "test treeSet" testTreeSet,
    testCase "test treeDelete" testTreeDelete,
    testCase "test mapDiff" testMapDiff
    ]

t1 = TConstant [CBool True]
t2 = TConstant [CBool False]
l1 = Leaf [] t1
l2 = Leaf [] t2
cempty = Container [] [] $ Map.empty
c1 = Container [] [] $ Map.singleton "b" l1
c2 = Container [] [] $ Map.singleton "a" c1

testTreeGet =
  do
    assertEqual "leaf get failed" (Right l1) $ treeGet ["b"] c1
    assertEqual "nested leaf get failed" (Right l1) $ treeGet ["a", "b"] c2
    assertEqual "container get failed" (Right c1) $ treeGet ["a"] c2
    assertFailed "didn't fail with too many path components" $
        treeGet ["a", "b", "c"] c2
    assertFailed "bad fail with bad keys" $ treeGet ["a", "llama"] c2
    assertFailed "bad fail with bad keys" $ treeGet ["llama"] c2
    assertFailed "bad fail with bad keys" $ treeGet ["llama", "face"] c2

testTreeDelete =
  do
    assertEqual "container delete failed" (Right cempty) $ treeDelete ["a"] c2
    assertEqual "leaf delete failed"
        (Right (Container [] [] $ Map.fromList [("a", cempty)])) $
        treeDelete ["a", "b"] c2
    assertFailed "bad path did not fail 1" $ treeDelete ["a", "llama"] c2
    assertFailed "bad path did not fail 2" $ treeDelete ["llama"] c2
    assertFailed "bad path did not fail 3" $ treeDelete ["llama", "face"] c2

testTreeAdd =
  do
    assertEqual "normal add failed" (Right c2') $ treeAdd l2 ["a", "c"] c2
    assertFailed "add existing failed" $ treeAdd l2 ["a", "b"] c2
  where
    c1' = Container [] [] $ Map.fromList [("b", l1), ("c", l2)]
    c2' = Container [] [] $ Map.singleton "a" c1'

testTreeSet =
  do
    assertEqual "normal set failed" (Right c2') $ treeSet l2 ["a", "b"] c2
    assertFailed "set non-eixstant failed" $ treeSet l2 ["a", "c"] c2
  where
    c1' = Container [] [] $ Map.singleton "b" l2
    c2' = Container [] [] $ Map.singleton "a" c1'


testMapDiff =
    assertEqual "failed mapDiff" expected $ mapDiff m1 m2
  where
    m1 = Map.fromList [('a', 1), ('b', 2)]
    m2 = Map.fromList [('a', 3), ('c', 4)]
    expected = Map.fromList [('a', Change 3), ('b', Remove), ('c', Add 4)]

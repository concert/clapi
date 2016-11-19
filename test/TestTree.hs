module TestTree where

import Util (assertFailed)
import Test.HUnit (assertEqual)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Data.Map.Strict as Map

import Types (ClapiValue(..), root)
import Tree (
    Tuple(..), Node(..), ClapiTree(..), treeGet, treeAdd, treeSet,
    treeDelete, mapDiff, applyMapDiff, Delta(..)
    )

tests = [
    testCase "treeGet" testTreeGet,
    testCase "treeAdd" testTreeAdd,
    testCase "treeSet" testTreeSet,
    testCase "treeDelete" testTreeDelete,
    testCase "mapDiff" testMapDiff,
    testProperty "diff roundtrip" testDiffRoundTrip
    ]

c1 = Container [] ["a"]
c2 = Container [] ["b"]
c2' = Container [] ["c", "b"]
l1 = Leaf [] 1
l2 = Leaf [] 2
t1 = Map.fromList [(root, c1), (["a"], c2), (["a", "b"], l1)]
t2 = Map.fromList [
    (root, c1), (["a"], c2'), (["a", "b"], l1), (["a", "c"], l2)]
cempty = Container [] []
tempty = Map.singleton root cempty

testTreeGet =
  do
    assertEqual "nested leaf get" (Right l1) $ treeGet ["a", "b"] t1
    assertEqual "container get" (Right c2) $ treeGet ["a"] t1
    assertFailed "too many path components" $ treeGet ["a", "b", "c"] t1
    assertFailed "bad keys" $ treeGet ["a", "llama"] t1

testTreeDelete =
  do
    assertEqual "container delete" (Right tempty) $ treeDelete ["a"] t1
    assertEqual "leaf delete"
        (Right (Map.fromList [([], c1), (["a"], cempty)])) $
        treeDelete ["a", "b"] t1
    assertFailed "bad path did not fail 1" $ treeDelete ["a", "llama"] t1
    -- assertFailed "bad path did not fail 2" $ treeDelete ["llama"] c2
    -- assertFailed "bad path did not fail 3" $ treeDelete ["llama", "face"] c2

testTreeAdd =
  do
    assertEqual "normal add" (Right t2) $ treeAdd l2 ["a", "c"] t1
    assertFailed "add at root" $ treeAdd l2 root t1
    assertFailed "add existing" $ treeAdd l2 ["a", "b"] t1
    assertFailed "add to non-existent parent" $ treeAdd l2 ["x", "b"] t1

testTreeSet =
  do
    assertEqual "normal set" (Right t1) $ treeSet l2 ["a", "b"] t1
    assertEqual "reorder keys" (Right t3) $ treeSet c2'' ["a"] t2
    assertFailed "change keys" $ treeSet c3 ["a"] t2
    assertFailed "change num keys" $ treeSet c1 ["a"] t2
    assertFailed "change leaf type" $ treeSet c4 ["a", "b"] t1
    assertFailed "change container type" $ treeSet c5 ["a"] t1
    assertFailed "set non-eixstant" $ treeSet l2 ["a", "c"] t1
    assertFailed "set in non-existent parent" $ treeSet l2 ["x"] t1
  where
    t1 = Map.fromList [(root, c1), (["a"], c2), (["a", "b"], l2)]
    t3 = Map.fromList [
      (root, c1), (["a"], c2''), (["a", "b"], l1), (["a", "c"], l2)]
    c2'' = Container [] ["b", "c"]
    c3 = Container [] ["x", "y"]
    c4 = Leaf ["foo"] 1
    c5 = Container ["foo"] ["a"]


testMapDiff =
    assertEqual "failed mapDiff" expected $ mapDiff m1 m2
  where
    m1 = Map.fromList [('a', 1), ('b', 2), ('d', 42)]
    m2 = Map.fromList [('a', 3), ('c', 4), ('d', 42)]
    expected = Map.fromList [('a', Change 3), ('b', Remove), ('c', Add 4)]

testDiffRoundTrip :: Map.Map Char Int -> Map.Map Char Int -> Bool
testDiffRoundTrip m1 m2 = applyMapDiff d m1 == m2
  where
    d = mapDiff m1 m2

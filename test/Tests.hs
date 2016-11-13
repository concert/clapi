{-# LANGUAGE OverloadedStrings #-}
module Tests where

import Data.Word (Word16)
import Test.HUnit ((@=?), assertBool, assertEqual)
import Test.QuickCheck (quickCheck)

import qualified Data.Map as Map

import Types (
    Time(..), ClapiValue(..), ClapiMessage(..), ClapiMethod(..), ClapiBundle,
    fromClapiValue, toClapiValue,
    Tuple(..), ClapiTree(..), treeGet, treeAdd, treeSet, treeDelete)
import Serialisation (encode, decode)


-- FIXME: we should define a QuickCheck.Arbitrary instance for ClapiValue and
-- use it to generate better random values
testClapiValueConversionRoundTrip = quickCheck propRoundTrip
  where
    propRoundTrip :: [Float] -> Bool
    propRoundTrip f = (fromClapiValue . toClapiValue) f == f

testBinarySerialisationRoundTrip =
    Right bundle @=? result where
        bundle = [message, message]
        message = CMessage
            ["hello", "world"]
            Error
            nestedArgList
            (zip [[c] | c <- ['a'..'z']] nestedArgList)
        argList = [
            CBool True, CBool False, CTime (Time 4 2),
            CWord32 32, CWord64 64, CInt32 (-32), CInt64 (-64), CFloat 15.1,
            CDouble 13.2, CString "Greetings Planet"]
        nestedArgList = (CList argList) : argList

        result = encode bundle >>= decode :: Either String ClapiBundle

testEncodeTooLongString =
    assertBool "Long string not detected" (didFail result)
    where
      didFail (Left _) = True
      didFail (Right _) = False
      n = fromIntegral $ (maxBound :: Word16)
      longStr = replicate (n + 1) 'a'
      result = encode longStr


t1 = TConstant [CBool True]
t2 = TConstant [CBool False]
l1 = Leaf [] t1
l2 = Leaf [] t2
cempty = Container [] $ Map.empty
c1 = Container [] $ Map.singleton "b" l1
c2 = Container [] $ Map.singleton "a" c1

testTreeGet =
  do
    assertEqual "leaf get failed" (Just l1) $ treeGet ["b"] c1
    assertEqual "nested leaf get failed" (Just l1) $ treeGet ["a", "b"] c2
    assertEqual "container get failed" (Just c1) $ treeGet ["a"] c2
    assertEqual "didn't fail with too many path components" Nothing $
        treeGet ["a", "b", "c"] c2
    assertEqual "bad fail with bad keys" Nothing $ treeGet ["a", "llama"] c2
    assertEqual "bad fail with bad keys" Nothing $ treeGet ["llama"] c2
    assertEqual "bad fail with bad keys" Nothing $ treeGet ["llama", "face"] c2

testTreeDelete =
  do
    assertEqual "container delete failed" (Just cempty) $ treeDelete ["a"] c2
    assertEqual "leaf delete failed"
        (Just (Container [] $ Map.fromList [("a", cempty)])) $
        treeDelete ["a", "b"] c2
    assertEqual "bad path did not fail 1" Nothing $ treeDelete ["a", "llama"] c2
    assertEqual "bad path did not fail 2" Nothing $ treeDelete ["llama"] c2
    assertEqual "bad path did not fail 3" Nothing $
        treeDelete ["llama", "face"] c2

testTreeAdd =
  do
    assertEqual "normal add failed" (Just c2') $ treeAdd l2 ["a", "c"] c2
    assertEqual "add existing failed" Nothing $ treeAdd l2 ["a", "b"] c2
  where
    c1' = Container [] $ Map.fromList [("b", l1), ("c", l2)]
    c2' = Container [] $ Map.singleton "a" c1'

testTreeSet =
  do
    assertEqual "normal set failed" (Just c2') $ treeSet l2 ["a", "b"] c2
    assertEqual "set non-eixstant failed" Nothing $ treeSet l2 ["a", "c"] c2
  where
    c1' = Container [] $ Map.singleton "b" l2
    c2' = Container [] $ Map.singleton "a" c1'

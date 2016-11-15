{-# LANGUAGE OverloadedStrings #-}
module Tests where

import Data.Word (Word16)
import Test.HUnit ((@=?), assertBool, assertEqual, Assertion)
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

assertFailed :: String -> Either a b -> Assertion
assertFailed s either = assertBool s (didFail either)
  where
    didFail (Left _) = True
    didFail (Right _) = False

testEncodeTooLongString =
    assertFailed "Long string not detected" $ encode longStr
    where
      n = fromIntegral $ (maxBound :: Word16)
      longStr = replicate (n + 1) 'a'


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

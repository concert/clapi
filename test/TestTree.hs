module TestTree where

import Util (assertFailed)
import Data.Either (isRight)
import Data.Foldable (toList)
import Control.Monad (liftM, liftM2, replicateM)
import Test.QuickCheck (
    Gen, Arbitrary(..), arbitrary, (==>), forAll, choose, elements, shuffle,
    oneof, frequency, listOf, vectorOf, sublistOf, counterexample, property)
import Test.QuickCheck.Property (Result(..), Property(..))
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import qualified Data.Map.Mos as Mos
import Path (Path(..), isChildOfAny)
import Types (CanFail, ClapiValue(..), Time(..), InterpolationType(..))
import Tree (
    Attributee, Site, SiteMap, TimeSeries, Node(..), ClapiTree(..),
    treeAdd, treeSet, treeDelete, treeDiff, treeApply
    )

tests = [
    testProperty "treeDiff round trip" testTreeDiffRoundTrip
    ]


instance Arbitrary Time where
    arbitrary = liftM2 Time arbitrary arbitrary

instance Arbitrary ClapiValue where
    arbitrary = oneof [
        CBool <$> arbitrary,
        CTime <$> arbitrary,
        CEnum <$> arbitrary,
        CWord32 <$> arbitrary,
        CWord64 <$> arbitrary,
        CInt32 <$> arbitrary,
        CInt64 <$> arbitrary,
        CFloat <$> arbitrary,
        CDouble <$> arbitrary,
        CString <$> arbitrary,
        clist]
      where
        clist =
          do
            len <- choose (0, 4)
            elems <- vectorOf len arbitrary
            return $ CList elems

instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary

instance Arbitrary InterpolationType where
    arbitrary = oneof $ return <$> [IConstant, ILinear, IBezier]

arbitraryAv :: (Arbitrary a) => Gen a -> Gen (Maybe Attributee, a)
arbitraryAv genA = do
    att <- oneof [return Nothing, liftM Just name]
    v <- genA
    return (att, v)

arbitraryMap :: (Ord k) =>
    Int -> Int -> Gen k -> (k -> Gen a) -> Gen (Map.Map k a)
arbitraryMap min max keyG itemG =
  do
    numItems <- choose (min, max)
    keys <- replicateM numItems keyG
    itemList <- sequence $ fmap (\k -> sequence (k, itemG k)) keys
    return $ Map.fromList itemList

submap :: (Ord k) => Map.Map k a -> Gen (Map.Map k a)
submap m = Map.fromList <$> (sublistOf $ Map.toList m)

slightlyDifferentMap :: (Ord k) => Gen (Map.Map k a) -> (a -> Gen a) ->
    Map.Map k a -> Gen (Map.Map k a)
slightlyDifferentMap g f m =
  do
    newBaseMap <- g
    reducedM <- submap m
    toTransform <- submap reducedM
    transformedM <- sequence $ fmap f toTransform
    return $ Map.union newBaseMap $ Map.union transformedM reducedM

arbitraryTimeSeries :: (Arbitrary a) => Maybe Site -> Gen (TimeSeries a)
arbitraryTimeSeries site =
    arbitraryMap 1 4 arbitrary (const $ arbitraryAv siteGen)
  where
    siteGen = case site of
      Nothing -> liftM Just arbitrary
      _ -> arbitrary

name = do
    l <- choose (1, 5)
    replicateM l $ elements ['a'..'z']

justName = oneof [return Nothing, liftM Just name]

arbitrarySiteMap :: (Arbitrary a) => Gen (SiteMap a)
arbitrarySiteMap = arbitraryMap 1 5 justName arbitraryTimeSeries

instance (Arbitrary a) => Arbitrary (Node a) where
    arbitrary =
      do
        numKeys <- choose (0, 4)
        keys <- replicateM numKeys name
        siteMap <- arbitrarySiteMap
        return $ Node keys siteMap


slightlyDifferentNode :: (Arbitrary a) => Node a -> Gen (Node a)
slightlyDifferentNode (Node keys sm) =
  do
    sm' <- slightlyDifferentMap arbitrarySiteMap (const arbitrary) sm
    sm'' <- return $ Map.filter (not . null) sm'
    keys' <- frequency [
        (1, sublistOf keys >>= shuffle),
        (3, return keys)
      ]
    return $ Node keys' sm''

arbitraryPath :: Gen Path
arbitraryPath = listOf name

instance (Arbitrary a) => Arbitrary (ClapiTree a) where
    arbitrary =
      do
        nm <- arbitraryMap 0 4 arbitraryPath (const arbitrary)
        tm <- sequence $ fmap (const arbitraryPath) nm
        return $ ClapiTree nm tm (Mos.invertMap tm)

slightlyDifferentTree :: (Arbitrary a) => ClapiTree a -> Gen (ClapiTree a)
slightlyDifferentTree (ClapiTree nm tm tum) =
  do
    nm' <- slightlyDifferentMap arbitrary slightlyDifferentNode nm
    deletedKeys <- return $ Set.toList $ Set.difference (Map.keysSet nm) (Map.keysSet nm')
    nm'' <- return $ Map.filterWithKey
        (\k _ -> not $ k `isChildOfAny` deletedKeys) nm'
    toDelta <- submap tm
    deltaTm <- sequence (fmap (const arbitrary) $ toDelta)
    deltaTm' <- return $ Map.union deltaTm tm
    arbTm <- sequence $ fmap (const arbitraryPath) nm''
    arbTm' <- return $ Map.intersection deltaTm' arbTm
    tm' <- return $ Map.union arbTm' arbTm
    return $ ClapiTree nm'' tm' (Mos.invertMap tm')

data TreePair a = TreePair (ClapiTree a) (ClapiTree a) deriving (Show)
instance (Arbitrary a) => Arbitrary (TreePair a) where
    arbitrary = do
        t1 <- arbitrary
        t2 <- slightlyDifferentTree t1
        return $ TreePair t1 t2
    shrink (TreePair (ClapiTree nm1 tm1 _) (ClapiTree nm2 tm2 _)) = do
        (np, n1) <- excludeSingleton $ Map.toList nm1
        tp1 <- get np tm1
        n2 <- get np nm2
        tp2 <- get np tm2
        return $ TreePair (make np n1 tp1) (make np n2 tp2)
      where
        excludeSingleton (a:[]) = []
        excludeSingleton as = as
        get k m = toList $ Map.lookup k m
        make np n tp = ClapiTree newNm newTm newTum
          where
            newNm = Map.singleton np n
            newTm = Map.singleton np tp
            newTum = Mos.invertMap newTm


testTreeDiffRoundTrip :: TreePair Int -> Property
testTreeDiffRoundTrip (TreePair t1 t2) =
    isRight d ==> counterexample (show d) $ gubbins failyT2'
      where
        d = treeDiff t1 t2
        failyT2' = d >>= treeApply t1
        gubbins (Left s) = counterexample s False
        gubbins (Right t2') = counterexample (show t2') $ t2' == t2

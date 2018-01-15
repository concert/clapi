{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
module TreeSpec where

import Test.Hspec
import Data.Either (isRight)
import Data.Foldable (toList)
import Control.Monad (liftM, liftM2, replicateM)
import Test.QuickCheck (
    Gen, Arbitrary(..), arbitrary, (==>), forAll, choose, elements, shuffle,
    oneof, frequency, listOf, vectorOf, sublistOf, counterexample, property)
import Test.QuickCheck.Property (Result(..), Property(..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import qualified Data.Map.Mos as Mos
import Clapi.TH
import Clapi.Types (
    CanFail, WireValue(..), Time(..), Interpolation(..))
import Clapi.Types.Path (Path(..), isChildOfAny, pattern Root)
import Clapi.Tree (
    ClapiTree, Node(..), treeOrphansAndMissing, treeInitNode, treeDeleteNode
    -- Attributee, Site, SiteMap, TimeSeries, Node(..), ClapiTree(..),
    -- treeAdd, treeSet, treeDelete, treeDiff, treeApply
    )

spec :: Spec
spec = do
    describe "Init node" $ do
        it "Remains contiguous" $ do
            assertContiguous t0
            assertContiguous t1
            assertContiguous t2
            assertContiguous t3
        it "Is as expected after 1 init" $
            t1 `shouldBe` expectedT1
        it "Is as expected after 3 inits" $
            t3 `shouldBe` expectedT3
    it "Deletes recursively" $
        treeDeleteNode [pathq|/a/d|] t3 `shouldReturn` t1

assertContiguous = flip shouldBe (mempty, mempty) . treeOrphansAndMissing

t0 = mempty :: ClapiTree ()
t1 = treeInitNode [pathq|/a/b/c|] t0
t2 = treeInitNode [pathq|/a/d/e|] t1
t3 = treeInitNode [pathq|/a/d|] t2

expectedT1 = Map.fromList [
    (Root, Node [[segq|a|]] mempty),
    ([pathq|/a|], Node [[segq|b|]] mempty),
    ([pathq|/a/b|], Node [[segq|c|]] mempty),
    ([pathq|/a/b/c|], Node [] mempty)] :: ClapiTree ()
expectedT3 = Map.fromList [
    (Root, Node [[segq|a|]] mempty),
    ([pathq|/a|], Node [[segq|b|], [segq|d|]] mempty),
    ([pathq|/a/b|], Node [[segq|c|]] mempty),
    ([pathq|/a/b/c|], Node [] mempty),
    ([pathq|/a/d|], Node [[segq|e|]] mempty),
    ([pathq|/a/d/e|], Node [] mempty)] :: ClapiTree ()


-- instance Arbitrary Time where
--     arbitrary = liftM2 Time arbitrary arbitrary

-- instance Arbitrary Interpolation where
--     arbitrary = oneof [
--       return IConstant, return ILinear, IBezier <$> arbitrary <*> arbitrary]

-- arbitraryAv :: (Arbitrary a) => Gen a -> Gen (Maybe Attributee, a)
-- arbitraryAv genA = do
--     att <- oneof [return Nothing, liftM Just name]
--     v <- genA
--     return (att, v)

-- arbitraryMap :: (Ord k) =>
--     Int -> Int -> Gen k -> (k -> Gen a) -> Gen (Map.Map k a)
-- arbitraryMap min max keyG itemG =
--   do
--     numItems <- choose (min, max)
--     keys <- replicateM numItems keyG
--     itemList <- sequence $ fmap (\k -> sequence (k, itemG k)) keys
--     return $ Map.fromList itemList

-- submap :: (Ord k) => Map.Map k a -> Gen (Map.Map k a)
-- submap m = Map.fromList <$> (sublistOf $ Map.toList m)

-- slightlyDifferentMap :: (Ord k) => Gen (Map.Map k a) -> (a -> Gen a) ->
--     Map.Map k a -> Gen (Map.Map k a)
-- slightlyDifferentMap g f m =
--   do
--     newBaseMap <- g
--     reducedM <- submap m
--     toTransform <- submap reducedM
--     transformedM <- sequence $ fmap f toTransform
--     return $ Map.union newBaseMap $ Map.union transformedM reducedM

-- arbitraryTimeSeries :: (Arbitrary a) => Maybe Site -> Gen (TimeSeries a)
-- arbitraryTimeSeries site =
--     arbitraryMap 1 4 arbitrary (const $ arbitraryAv siteGen)
--   where
--     siteGen = case site of
--       Nothing -> liftM Just arbitrary
--       _ -> arbitrary

-- name = do
--     l <- choose (1, 5)
--     replicateM l $ elements ['a'..'z']

-- arbitrarySiteMap :: (Arbitrary a) => Gen (SiteMap a)
-- arbitrarySiteMap = arbitraryMap 1 5 justName arbitraryTimeSeries
--   where
--     justName = oneof [return Nothing, liftM Just name]


-- instance (Arbitrary a) => Arbitrary (Node a) where
--     arbitrary =
--       do
--         numKeys <- choose (0, 4)
--         keys <- replicateM numKeys name
--         siteMap <- arbitrarySiteMap
--         return $ Node keys siteMap


-- slightlyDifferentNode :: (Arbitrary a) => Node a -> Gen (Node a)
-- slightlyDifferentNode (Node keys sm) =
--   do
--     sm' <- slightlyDifferentMap arbitrarySiteMap (const arbitrary) sm
--     sm'' <- return $ Map.filter (not . null) sm'
--     keys' <- frequency [
--         (1, sublistOf keys >>= shuffle),
--         (3, return keys)
--       ]
--     return $ Node keys' sm''

-- arbitraryPath :: Gen Path
-- arbitraryPath = listOf name

-- instance (Arbitrary a) => Arbitrary (ClapiTree a) where
--     arbitrary =
--       do
--         nm <- arbitraryMap 0 4 arbitraryPath (const arbitrary)
--         tm <- sequence $ fmap (const arbitraryPath) nm
--         return $ ClapiTree nm (tm, Mos.invertMap tm)

-- slightlyDifferentTree :: (Arbitrary a) => ClapiTree a -> Gen (ClapiTree a)
-- slightlyDifferentTree (ClapiTree nm (tm, _)) =
--   do
--     nm' <- slightlyDifferentMap arbitrary slightlyDifferentNode nm
--     deletedKeys <- return $ Set.toList $ Set.difference (Map.keysSet nm) (Map.keysSet nm')
--     nm'' <- return $ Map.filterWithKey
--         (\k _ -> not $ k `isChildOfAny` deletedKeys) nm'
--     toDelta <- submap tm
--     deltaTm <- sequence (fmap (const arbitrary) $ toDelta)
--     deltaTm' <- return $ Map.union deltaTm tm
--     arbTm <- sequence $ fmap (const arbitraryPath) nm''
--     arbTm' <- return $ Map.intersection deltaTm' arbTm
--     tm' <- return $ Map.union arbTm' arbTm
--     return $ ClapiTree nm'' (tm', Mos.invertMap tm')

-- data TreePair a = TreePair (ClapiTree a) (ClapiTree a) deriving (Show)
-- instance (Arbitrary a) => Arbitrary (TreePair a) where
--     arbitrary = do
--         t1 <- arbitrary
--         t2 <- slightlyDifferentTree t1
--         return $ TreePair t1 t2
--     shrink (TreePair (ClapiTree nm1 types1) (ClapiTree nm2 types2)) = do
--         (np, n1) <- excludeSingleton $ Map.toList nm1
--         tp1 <- toList $ Mos.getDependency np types1
--         n2 <- toList $ Map.lookup np nm2
--         tp2 <- toList $ Mos.getDependency np types2
--         return $ TreePair (make np n1 tp1) (make np n2 tp2)
--       where
--         excludeSingleton (a:[]) = []
--         excludeSingleton as = as
--         make np n tp = ClapiTree newNm (newTm, newTum)
--           where
--             newNm = Map.singleton np n
--             newTm = Map.singleton np tp
--             newTum = Mos.invertMap newTm


-- propTreeDiffRoundTrip :: TreePair Int -> Property
-- propTreeDiffRoundTrip (TreePair t1 t2) =
--     isRight d ==> counterexample (show d) $ gubbins failyT2'
--       where
--         d = treeDiff t1 t2
--         failyT2' = d >>= treeApply t1
--         gubbins (Left s) = counterexample s False
--         gubbins (Right t2') = counterexample (show t2') $ t2' == t2

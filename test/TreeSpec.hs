{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
module TreeSpec where

import Test.Hspec

import Clapi.TH
import Clapi.Types.AssocList (alSingleton, alFromList)
import qualified Clapi.Types.Dkmap as Dkmap
import Clapi.Types.Path (Seg, pattern Root, pattern (:/))
import Clapi.Tree
  ( RoseTree(..), RoseTreeNode(..), treePaths, treeLookup, treeInsert
  , treeDelete, treeLookupNode)

s0, s1, s2, s3, s4 :: Seg
s0 = [segq|t0|]
s1 = [segq|t1|]
s2 = [segq|t2|]
s3 = [segq|t3|]
s4 = [segq|t4|]

t0, t1, t2, t3, t4 :: RoseTree Char
t0 = RtEmpty
t1 = RtConstData Nothing 'b'
t2 = RtDataSeries Dkmap.empty
t3 = RtContainer $ fmap (Nothing,) $ alFromList [(s0, t0), (s1, t1), (s2, t2)]
t4 = RtContainer $ fmap (Nothing,) $ alFromList [(s3, t3), (s1, t1)]

spec :: Spec
spec = do
  describe "treePaths" $
    it "should return the paths of all the nodes in a tree" $
      treePaths Root t4 `shouldBe`
        [ Root
        , Root :/ s3
        , Root :/ s3 :/ s0
        , Root :/ s3 :/ s1
        , Root :/ s3 :/ s2
        , Root :/ s1]

  describe "treeLookup" $ do
    it "should find a node that is present" $
      treeLookup (Root :/ s3 :/ s1) t4 `shouldBe` Just t1
    it "should return Nothing if no node is present" $
      treeLookup [pathq|/naff|] t4 `shouldBe` Nothing

  describe "treeInsert" $  do
    it "should insert the node" $
      let
        att = Just "bob"
        t = treeInsert att (Root :/ s2) t2 t4
        expectedKids = alFromList [(s3, Nothing), (s1, Nothing), (s2, att)]
      in do
        treeLookup (Root :/ s2) t `shouldBe` Just t2
        treeLookupNode Root t `shouldBe` Just (RtnChildren expectedKids)

    it "should overwrite an existing node" $
      let
        att = Just "bob"
        t = treeInsert att (Root :/ s1) t2 t4
        expectedKids = alFromList [(s3, Nothing), (s1, att)]
      in do
        treeLookup (Root :/ s1) t `shouldBe` Just t2
        treeLookupNode Root t `shouldBe` Just (RtnChildren expectedKids)

    it "should recursively add containers as needed" $
      let
        att = Just "bob"
        t = treeInsert att ([pathq|/will/bo|]) t0 t1
        expectedT = RtContainer $ alSingleton [segq|will|]
          (att, RtContainer $ alSingleton [segq|bo|] (att, t0))
      in
        t `shouldBe` expectedT

  describe "treeDelete" $ do
    it "should delete something that isn't there" $ do
      treeDelete [pathq|/will|] t4 `shouldBe` t4
      treeDelete [pathq|/will|] t1 `shouldBe` t1
    it "should delete recursively" $
      treeDelete (Root :/ s3 :/ s1) t4 `shouldBe` RtContainer (alFromList
        [ (s3, (Nothing, RtContainer $ alFromList
          [(s0, (Nothing, t0)), (s2, (Nothing, t2))]))
        , (s1, (Nothing, t1))
        ])
    it "doesn't create illegitimate parents" $ do
      treeDelete [pathq|/t3/to/box|] t3 `shouldBe` t3
      treeDelete [pathq|/t1/to/box|] t3 `shouldBe` t3

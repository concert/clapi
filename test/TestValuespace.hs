module TestValuespace where

import Test.HUnit (assertEqual)
import Test.Framework.Providers.HUnit (testCase)

import Data.Maybe (isNothing)
import Data.Word (Word16)
import qualified Data.Map.Strict as Map

import Valuespace (getBaseValuespace, getTree)
import Tree (treeOrphansAndMissing)

tests = [
    testCase "test base valuespace" testBaseValuespace
    ]

testBaseValuespace = assertEqual "clean base valuespace" (mempty, mempty) $
    treeOrphansAndMissing baseVsTree
    -- FIXME: this misses latent errors in the values because of laziness
  where
    baseVsTree = getTree getBaseValuespace

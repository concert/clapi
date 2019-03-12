module SequenceOpsSpec where

import Test.Hspec
import Test.QuickCheck (property)

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Clapi.Types.AssocList as AL
import Clapi.Types.SequenceOps
  ( fullOrderOps, dependencyOrder, unDependencyOrdered, updateUniqList
  , validateAfters)
import Clapi.Types.UniqList (UniqList(..))

import Arbitrary ()

spec :: Spec
spec = do
    describe "fullOrderOps" $ do
        it "outputs in dependency order" $ property $
          \(ul :: UniqList Int) -> do
            ordered <- dependencyOrder $ AL.toMap $ unDependencyOrdered
              $ fullOrderOps ul
            fullOrderOps ul `shouldBe` ordered
        it "generates instructions to build the original list" $ property $
          \(ul :: UniqList Int) -> do
            generated <- updateUniqList id (fullOrderOps ul) mempty
            generated `shouldBe` ul
    describe "validateAfters" $ do
        it "orders correctly structured afters" $
            Set.fromList <$> validateAfters (Map.fromList
              [('a', 'c'), ('b', 'e'), ('c', 'd'), ('e', 'a'), ('x', 'y')])
            `shouldBe` Just (Set.fromList ["dcaeb", "yx"])
        it "catches duplicate references" $
            Set.fromList <$> validateAfters (Map.fromList
              [('a', 'b'), ('b', 'c'), ('d', 'c')])
            `shouldBe` Nothing
        it "catches cyclic references" $ do
            Set.fromList <$> validateAfters (Map.fromList
              [('a', 'b'), ('b', 'c'), ('c', 'a')])
              `shouldBe` Nothing
            Set.fromList <$> validateAfters (Map.fromList
              [('a', 'b'), ('b', 'c'), ('c', 'b')])
              `shouldBe` Nothing

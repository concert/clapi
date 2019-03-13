module SequenceOpsSpec where

import Test.Hspec
import Test.QuickCheck (property)

import Control.Monad.Except (runExcept)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Clapi.Types.AssocList as AL
import Clapi.Types.SequenceOps
  ( DependencyError(..), fullOrderOps, dependencyOrder, unDependencyOrdered
  , updateUniqList, extractDependencyChains)
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
    describe "extractDependencyChains" $ do
        it "orders correctly structured afters" $
            extractDependencyChains' (Map.fromList
              [('a', 'c'), ('b', 'e'), ('c', 'd'), ('e', 'a'), ('x', 'y')])
            `shouldBe` Right (Set.fromList [[('c', 'd'), ('a', 'c'), ('e', 'a'), ('b', 'e')], [('x', 'y')]])
        it "catches duplicate references" $
            extractDependencyChains' (Map.fromList
              [('a', 'b'), ('b', 'c'), ('d', 'c')])
            `shouldBe`
            Left (DuplicateReferences $ Map.singleton 'c' $ Set.fromList "bd")
        it "catches cyclic references" $
            extractDependencyChains' (Map.fromList
              [('a', 'b'), ('b', 'c'), ('c', 'a'), ('x', 'y'), ('y', 'x')])
              `shouldBe` Left (CyclicReferences [[('c', 'a'), ('b', 'c'), ('a', 'b')], [('y', 'x'), ('x', 'y')]])

extractDependencyChains'
  :: Ord a => Map a a -> Either (DependencyError a) (Set [(a, a)])
extractDependencyChains'
  = fmap Set.fromList . runExcept . extractDependencyChains

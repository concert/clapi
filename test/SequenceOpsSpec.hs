module SequenceOpsSpec where

import Test.Hspec
import Test.QuickCheck (property)

import Control.Monad.Except (runExcept)
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
      extractDependencyChains' ["ac", "be", "cd", "ea", "xy"]
      `shouldBe`
      Right (Set.fromList [mkPairs ["cd", "ac", "ea", "be"], mkPairs ["xy"]])
    it "catches duplicate references" $
      extractDependencyChains' ["ab", "bc", "dc"]
      `shouldBe`
      Left (DuplicateReferences [('c', "bd")])
    it "catches cyclic references" $
      extractDependencyChains' ["ab", "bc", "ca", "xy", "yx"]
      `shouldBe`
      Left (CyclicReferences [mkPairs ["ca", "bc", "ab"], mkPairs ["yx", "xy"]])


-- Just to make the test code above less noisy ;-)
mkPairs :: [[a]] -> [(a, a)]
mkPairs = fmap (\[a1, a2] -> (a1, a2))

extractDependencyChains'
  :: Ord a => [[a]] -> Either (DependencyError a a) (Set [(a, a)])
extractDependencyChains'
    = fmap Set.fromList . runExcept . extractDependencyChains id . Map.fromList
      . mkPairs

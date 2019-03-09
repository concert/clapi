module SequenceOpsSpec where

import Test.Hspec
import Test.QuickCheck (property)

import qualified Clapi.Types.AssocList as AL
import Clapi.Types.SequenceOps
  ( fullOrderOps, dependencyOrder, unDependencyOrdered, updateUniqList)
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

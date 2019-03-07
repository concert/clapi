module SequenceOpsSpec where

import Test.Hspec
import Test.QuickCheck (property)

import qualified Clapi.Types.AssocList as AL
import Clapi.Types.SequenceOps (fullOrderOps, dependencyOrder, updateUniqList)
import Clapi.Types.UniqList (UniqList(..))

import Arbitrary ()

spec :: Spec
spec = do
    describe "fullOrderOps" $ do
        it "outputs in dependency order" $ property $ \(ul :: UniqList Int) ->
          let list = unUniqList ul in do
            ordered <- dependencyOrder $ AL.toMap $ fullOrderOps list
            fullOrderOps list `shouldBe` ordered
        it "generates instructions to build the original list" $ property $ \(ul :: UniqList Int) ->
          let
            list = unUniqList ul
            ops = fullOrderOps list
          in do
            generated <- updateUniqList (AL.toMap ops) mempty
            generated `shouldBe` ul

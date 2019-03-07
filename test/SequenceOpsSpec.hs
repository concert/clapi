module SequenceOpsSpec where

import Test.Hspec
import Test.QuickCheck (property)

import qualified Clapi.Types.AssocList as AL
import Clapi.Types.SequenceOps (fullOrderOps, dependencyOrder)
import Clapi.Types.UniqList (UniqList(..))

import Arbitrary ()

spec :: Spec
spec = do
    describe "fullOrderOps" $ do
        it "outputs in dependency order" $ property $ \(ul :: UniqList Int) ->
          let list = unUniqList ul in do
            ordered <- dependencyOrder $ AL.toMap $ fullOrderOps list
            fullOrderOps list `shouldBe` ordered

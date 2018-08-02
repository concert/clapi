{-# LANGUAGE
    OverloadedStrings
#-}

module TextSerialisationSpec where

import Test.Hspec
import Test.QuickCheck (property)

import Clapi.TextSerialisation (ttFromText, ttToText)
import Clapi.Types ()
import TypesSpec ()

spec :: Spec
spec = do
    describe "Tree type descriptions" $ do
        it "should survive a round trip to text" $ property $
            \tt -> either error id (ttFromText (ttToText tt)) `shouldBe` tt
        it "should fail to deserialise nonsense" $
          ttFromText "this is not a type" `shouldBe` Nothing

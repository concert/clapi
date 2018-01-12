{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
module ValuespaceSpec where

import Test.Hspec
import Test.QuickCheck (
    Arbitrary(..), Gen, Property, arbitrary, oneof, elements, listOf, listOf1,
    arbitraryBoundedEnum, vector, vectorOf, property)
import Test.QuickCheck.Instances ()

import Data.Maybe (fromJust)
import Data.Either (either)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Data.Int
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Lens (view)
import Control.Monad (replicateM)
import Control.Monad.Fail (MonadFail)

import Clapi.Path (Path(..), pattern (:/), mkSeg, Seg)
import Clapi.PathQ
import Clapi.Types.AssocList (alFromMap, AssocList, mkAssocList)
import Clapi.Types (
  InterpolationType, Interpolation(..), WireValue(..), TreeType(..),
  TreeConcreteType(..))
import Clapi.Validator (validate)
import Clapi.Valuespace

import TypesSpec () -- For Arbitrary instances
import TextSerialisationSpec ()

spec :: Spec
spec = do
    describe "Validation" $ do
        it "baseValuespace valid" $ (fst $ vsValidate $ ownerUnlock baseValuespace) `shouldBe` mempty
        it "detects child type error" $ do
            -- Set /api/self/version to have the wrong type, but perfectly valid
            -- data for that wrong type:
            badVs <- vsSet
                  anon IConstant
                  [WireValue @Text "doccy", WireValue @[Text] [], WireValue @[Text] [], WireValue @[Word8] [0]]
                  [pathq|/api/self/version|]
                  globalSite tconst (ownerUnlock baseValuespace) >>=
                return . vsAssignType [pathq|/api/self/version|]
                    [pathq|/api/types/base/tuple|]
            assertValidationErrors [[pathq|/api/self|]] badVs
        it "rechecks on type def changes" $ do
          do
            -- Make sure changing /api/types/self/version goes and checks things defined
            -- to have that type:
            newDef <- TupleDefinition "for test" <$>
              mkAssocList [([segq|versionString|], TtConc $ TcString "apple")]
              <*> pure mempty
            badVs <- vsSet anon IConstant (toWireValues newDef)
                [pathq|/api/types/self/version|] globalSite tconst
                (ownerUnlock baseValuespace)
            assertValidationErrors [[pathq|/api/self/version|]] badVs
        it "xref" $ do
            -- Change the type of the instance referenced in a cross reference
            vs <- vsWithXRef
            assertValidationErrors [] vs
            badVs <- vsSet anon IConstant
                [WireValue @Text "/api/types/base/tuple"]
                [pathq|/api/types/test_value|] globalSite tconst vs
            assertValidationErrors [[pathq|/api/types/test_value|]] badVs
        it "xref revalidation" $ do
            newDef <- StructDefinition "for test" <$> mkAssocList
              [ ([segq|version|], ([pathq|/api/types/base/tuple|], Cannot)) ]
            badVs <- vsWithXRef >>=
                vsSet anon IConstant (toWireValues newDef)
                    [pathq|/api/types/containers/self|] globalSite tconst >>=
                vsSet anon IConstant [WireValue @Text "banana", WireValue @[Text] [], WireValue @[Text] [], WireValue @[Word8] [0]] [pathq|/api/self/version|]
                    globalSite tconst
            assertValidationErrors [[pathq|/api/types/test_value|]] badVs
    it "Array" $ do
        let newDef = ArrayDefinition "for test" [pathq|/api/types/self/version|] Cannot
        (evs, testPath) <- extendedVs newDef
        assertValidationErrors [[pathq|/api/types|]] evs
        emptyArrayVs <- vsSetChildren testPath [] evs
        assertValidationErrors [] emptyArrayVs
        missingEntryVs <- vsSetChildren testPath [[segq|a|]] emptyArrayVs
        assertValidationErrors [testPath] missingEntryVs
        filledEntryVs <- vsAdd anon IConstant
            [WireValue @Word32 0, WireValue @Int32 12]
            (testPath :/ [segq|a|]) globalSite tconst missingEntryVs
        assertValidationErrors [] filledEntryVs
    it "Doesn't get stuck with cyclic inference" $ do
        outerType <- StructDefinition "outer" <$> mkAssocList
            [ ([segq|defs|], ([pathq|/api/cyclic/defs/cyclic|], Cannot)) ]
        innerType <- StructDefinition "inner" <$> mkAssocList
            [ ([segq|cyclic|], ([pathq|/api/types/base/struct|], Cannot))
            , ([segq|defs|], ([pathq|/api/types/base/struct|], Cannot))]
        newCDef <- StructDefinition "updated for test" <$> mkAssocList
            [ ([segq|self|], ([pathq|/api/types/containers/self|], Cannot))
            , ([segq|types|], ([pathq|/api/types/containers/types|], Cannot))
            , ([segq|cyclic|], ([pathq|/api/cyclic/defs/cyclic|], Cannot))
            ]
        badVs <- vsSet anon IConstant (toWireValues newCDef)
              [pathq|/api/types/containers/api|] globalSite tconst
              (ownerUnlock baseValuespace) >>=
            addDef [pathq|/api/cyclic/defs/cyclic|] outerType >>=
            addDef [pathq|/api/cyclic/defs/defs|] innerType
        assertValidationErrors [
            [pathq|/api/cyclic|], [pathq|/api/cyclic/defs|],
            [pathq|/api/cyclic/defs/cyclic|], [pathq|/api/cyclic/defs/defs|]]
            badVs
    it "Definition <-> WireValue round trips" $ property $ \d ->
        (valuesToDef (defDispatch metaType d) . defDispatch toWireValues) d == Just d
  where
    assertValidationErrors errPaths vs = let (errs, _vvs) = vsValidate vs in
        Map.keysSet errs `shouldBe` Set.fromList errPaths


extendedVs :: (MonadFail m, OfMetaType def) => def -> m (Valuespace OwnerUnvalidated, Path)
extendedVs def = do
    newCDef <- StructDefinition "updated for test" <$> mkAssocList
        [ ([segq|base|], ([pathq|/api/types/containers/base|], Cannot))
        , ([segq|self|], ([pathq|/api/types/containers/types_self|], Cannot))
        , ([segq|containers|], ([pathq|/api/types/containers/containers|], Cannot))
        , ([segq|test_type|], (metaTypePath $ metaType def, Cannot))
        , ([segq|test_value|], ([pathq|/api/types/test_type|], Cannot))]
    vs <- vsAdd anon IConstant (toWireValues def) [pathq|/api/types/test_type|]
          globalSite tconst (ownerUnlock baseValuespace) >>=
        vsSet anon IConstant (toWireValues newCDef)
          [pathq|/api/types/containers/types|] globalSite tconst
    return (vs, [pathq|/api/types/test_value|])


vsWithXRef :: (MonadFail m) => m (Valuespace OwnerUnvalidated)
vsWithXRef =
  do
    newNodeDef <- TupleDefinition "for test" <$>
      mkAssocList [
        ([segq|daRef|], TtConc $ TcRef [pathq|/api/types/self/version|])] <*>
      pure mempty
    (vs, testPath) <- extendedVs newNodeDef
    vsAdd
      anon IConstant [WireValue @Text "/api/self/version"] testPath globalSite
      tconst vs


addDef ::
    (MonadFail m, OfMetaType def) => Path -> def ->
    Valuespace OwnerUnvalidated -> m (Valuespace OwnerUnvalidated)
addDef p d = vsAdd anon IConstant (toWireValues d) p globalSite tconst

instance Arbitrary Liberty where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary InterpolationType where
    arbitrary = arbitraryBoundedEnum

-- http://stackoverflow.com/questions/9542313/
infiniteStrings :: Int -> [String]
infiniteStrings minLen = [minLen..] >>= (`replicateM` ['a'..'z'])

uniqueSegs :: Int -> [Seg]
uniqueSegs n = take n $ fromJust . mkSeg . T.pack <$> infiniteStrings 4

instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (AssocList a b) where
  arbitrary = alFromMap <$> arbitrary

instance Arbitrary Definition where
    arbitrary =
      do
        oneof
          [ TupleDef <$>
              (TupleDefinition <$> arbitrary <*> arbitrary <*> arbitrary)
          , StructDef <$>
              (StructDefinition <$> arbitrary <*> arbitrary)
          , ArrayDef <$>
              (ArrayDefinition <$> arbitrary <*> arbitrary <*> arbitrary)
          ]

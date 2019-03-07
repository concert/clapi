{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    DataKinds
  , FlexibleInstances
  , GADTs
  , GeneralizedNewtypeDeriving
  , OverloadedStrings
  , RankNTypes
  , StandaloneDeriving
  , TemplateHaskell
  , TypeSynonymInstances
#-}
module Arbitrary where

import Test.QuickCheck
  ( Arbitrary(..), Gen
  , arbitraryBoundedEnum, choose, elements, listOf, oneof, shuffle)

import Control.Monad (replicateM)
import Data.Constraint (Dict(..))
import Data.List (inits)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Language.Haskell.TH (Type(ConT))
import System.Random (Random)

import Data.Map.Mol (Mol)
import qualified Data.Map.Mol as Mol
import Data.Map.Mos (Mos)
import qualified Data.Map.Mos as Mos

import Clapi.TextSerialisation (argsOpen, argsClose)
import Clapi.Types
import Clapi.Types.SequenceOps (SequenceOp(..))
import qualified Clapi.Types.SymbolList as SL
import Clapi.Types.AssocList (AssocList)
import qualified Clapi.Types.AssocList as AL
import Clapi.Types.WireTH (mkGetWtConstraint)


boundedListOf :: Int -> Int -> Gen a -> Gen [a]
boundedListOf lo hi g = choose (lo, hi) >>= flip replicateM g

smallListOf :: Gen a -> Gen [a]
smallListOf = boundedListOf 0 5

smallListOf1 :: Gen a -> Gen [a]
smallListOf1 = boundedListOf 1 5

smallMapOf :: Ord k => Gen k -> Gen v -> Gen (Map k v)
smallMapOf gk gv = Map.fromList <$> smallListOf ((,) <$> gk <*> gv)

smallMap :: (Ord k, Arbitrary k, Arbitrary v) => Gen (Map k v)
smallMap = smallMapOf arbitrary arbitrary

instance (Ord k, Ord v, Arbitrary k, Arbitrary v) => Arbitrary (Mos k v) where
  arbitrary = Mos.fromMap <$> smallMapOf arbitrary arbitrary
  shrink =
    fmap (Mos.fromList . Map.foldMapWithKey (\k -> fmap (k,) . Set.toList))
    . traverse shrink
    . Mos.unMos

genMol :: Ord k => Gen k -> Gen [v] -> Gen (Mol k v)
genMol k vs = Mol.fromMap <$> smallMapOf k vs

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Mol k v) where
  arbitrary = genMol arbitrary arbitrary
  shrink = fmap Mol.fromMap . traverse shrink . Mol.unMol

instance (Ord a, Arbitrary a) => Arbitrary (UniqList a) where
  arbitrary =
     arbitrary >>= shuffle . Set.toList >>= return . unsafeMkUniqList

assocListOf :: Ord k => Gen k -> Gen v -> Gen (AssocList k v)
assocListOf gk gv =
  smallMapOf gk gv >>= shuffle . Map.toList >>= return . AL.unsafeMkAssocList

instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (AssocList a b) where
  arbitrary = assocListOf arbitrary arbitrary
  shrink = fmap AL.unsafeMkAssocList . shrink . AL.unAssocList


arbitraryTextNoNull :: Gen Text
arbitraryTextNoNull = Text.pack . filter (/= '\NUL') <$>
  boundedListOf 0 30 arbitrary

instance Arbitrary Text where
  arbitrary = arbitraryTextNoNull
  shrink = fmap Text.pack . shrink . Text.unpack


name :: Gen (Name nr)
name = fromJust . mkName . Text.pack <$> smallListOf1 (elements ['a'..'z'])

instance Arbitrary (Name nr) where
  arbitrary = name

deriving instance Arbitrary Attributee

instance Arbitrary a => Arbitrary (Path' a) where
  arbitrary = Path' <$> smallListOf arbitrary
  shrink (Path' names) = fmap Path' . drop 1 . reverse . inits $ names

instance Arbitrary Editability where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary InterpolationType where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Time where
  arbitrary = Time <$> arbitrary <*> arbitrary
  shrink = \case
    Time 0 0 -> []
    Time x y -> [Time x' y' | (x', y') <- shrink (x, y)]

instance Arbitrary Interpolation where
  arbitrary = oneof
      [ return IConstant
      , return ILinear
      , IBezier <$> arbitrary <*> arbitrary]
  shrink = \case
    IBezier _ _ -> [IConstant]
    _ -> []

mkBounds :: Ord a => Maybe a -> Maybe a -> Bounds a
mkBounds a b = either error id $ case (a, b) of
  (Just _, Just _) -> bounds (min a b) (max a b)
  _ -> bounds a b

instance (Ord a, Random a, Arbitrary a) => Arbitrary (Bounds a) where
    arbitrary = mkBounds <$> arbitrary <*> arbitrary
    shrink b = [mkBounds lo hi | (lo, hi) <- shrink (boundsMin b, boundsMax b)]

arbitraryRegex :: Gen Text
arbitraryRegex =
  let
    niceAscii = pure @[] <$> ['!'..'Z'] ++ ['^'..'~']
    escapedArgsDelims = ('\\':) . pure @[] <$> [argsOpen, argsClose]
  in do
    safe <- listOf (elements $ niceAscii ++ escapedArgsDelims)
    delims <- flip replicate "[]" <$> choose (0, 3)
    Text.pack . mconcat <$> shuffle (safe ++ delims)

instance Arbitrary SomeTreeType where
  arbitrary = oneof
    [ return ttTime
    , ttEnum . fmap (Text.unpack . unName) <$> arbitrary
    , ttWord32 <$> arbitrary, ttWord64 <$> arbitrary
    , ttInt32 <$> arbitrary, ttInt64 <$> arbitrary
    , ttFloat <$> arbitrary, ttDouble <$> arbitrary
    , ttString <$> arbitraryRegex
    , ttRef <$> arbitrary
    , ttList <$> arbitrary, ttSet <$> arbitrary, ttOrdSet <$> arbitrary
    , ttMaybe <$> arbitrary
    , ttPair <$> arbitrary <*> arbitrary
    ]
  shrink (SomeTreeType tt) = case tt of
    TtTime -> []
    TtEnum sl -> ttEnum <$> shrink (SL.toStrings sl)
    TtWord32 b -> ttWord32 <$> shrink b
    TtWord64 b -> ttWord64 <$> shrink b
    TtInt32 b -> ttInt32 <$> shrink b
    TtInt64 b -> ttInt64 <$> shrink b
    TtFloat b -> ttFloat <$> shrink b
    TtDouble b -> ttDouble <$> shrink b
    TtString r -> case r of
      "" -> []
      _ -> [ttString ""]
    TtRef _ -> []
    TtList tt1 -> ttList <$> shrink (SomeTreeType tt1)
    TtSet tt1 -> ttSet <$> shrink (SomeTreeType tt1)
    TtOrdSet tt1 -> ttOrdSet <$> shrink (SomeTreeType tt1)
    TtMaybe tt1 -> ttMaybe <$> shrink (SomeTreeType tt1)
    TtPair tt1 tt2 ->
      uncurry ttPair <$> shrink (SomeTreeType tt1, SomeTreeType tt2)

instance Arbitrary SomeWireType where
  arbitrary = oneof
    [ return wtTime
    , return wtWord32, return wtWord64
    , return wtInt32, return wtInt64
    , return wtFloat, return wtDouble
    , return wtString
    , wtList <$> arbitrary, wtMaybe <$> arbitrary
    , wtPair <$> arbitrary <*> arbitrary
    ]

mkGetWtConstraint "getArbitrary" $ ConT ''Arbitrary

genWv :: WireType wt -> Gen (WireValue wt)
genWv wt = WireValue wt <$> go wt
  where
    go :: forall wt. WireType wt -> Gen wt
    go = \case
      WtMaybe wt1 -> oneof [return Nothing, Just <$> go wt1]
      WtList wt1 -> smallListOf $ go wt1
      WtPair wt1 wt2 -> (,) <$> go wt1 <*> go wt2
      wt1 -> case getArbitrary wt1 of
        Dict -> arbitrary

instance Arbitrary SomeWireValue where
  arbitrary = do
      SomeWireType wt <- arbitrary
      SomeWireValue <$> genWv wt
  shrink (SomeWireValue (WireValue wt a)) = case getArbitrary wt of
    Dict -> someWv wt <$> shrink a


instance Arbitrary PostDefinition where
  arbitrary = PostDefinition <$> arbitrary <*>
    assocListOf arbitrary (smallListOf arbitrary)

instance Arbitrary (Definition 'Tuple) where
  arbitrary = TupleDef <$> arbitrary <*> arbitrary <*> arbitrary
  shrink (TupleDef d ty ilim) =
    [TupleDef d' ty' ilim' | (d', ty', ilim') <- shrink (d, ty, ilim)]

instance Arbitrary (Definition 'Struct) where
  arbitrary = StructDef <$> arbitrary <*> arbitrary
  shrink (StructDef d tyinfo) =
    [StructDef d' tyinfo' | (d', tyinfo') <- shrink (d, tyinfo)]

instance Arbitrary (Definition 'Array) where
  arbitrary = ArrayDef <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (ArrayDef d ptn tn ed) =
    [ArrayDef d' ptn' tn' ed' | (d', ptn', tn', ed') <- shrink (d, ptn, tn, ed)]

instance Arbitrary SomeDefinition where
  arbitrary = oneof
    [ SomeDefinition <$> arbitrary @(Definition 'Tuple)
    , SomeDefinition <$> arbitrary @(Definition 'Struct)
    , SomeDefinition <$> arbitrary @(Definition 'Array)
    ]
  shrink (SomeDefinition def) = case def of
    TupleDef {} -> SomeDefinition <$> shrink def
    StructDef {} -> SomeDefinition <$> shrink def
    ArrayDef {} -> SomeDefinition <$> shrink def

instance Arbitrary CreateOp where
  arbitrary = OpCreate <$> smallListOf (smallListOf arbitrary) <*> arbitrary
  shrink (OpCreate args after) =
    [OpCreate args' after' | (args', after') <- shrink (args, after)]

instance Arbitrary d => Arbitrary (DefOp d) where
  arbitrary = oneof [OpDefine <$> arbitrary, return OpUndefine]
  shrink = \case
    OpUndefine -> []
    OpDefine def -> OpUndefine : (OpDefine <$> shrink def)

instance Arbitrary TimeSeriesDataOp where
  arbitrary = oneof
    [ OpSet <$> arbitrary <*> smallListOf arbitrary <*> arbitrary
    , return OpRemove]
  shrink = \case
    OpRemove -> []
    OpSet t wvs i ->
      OpRemove : [OpSet t' wvs' i' | (t', wvs', i') <- shrink (t, wvs, i)]

instance Arbitrary DataChange where
  arbitrary = oneof
    [ ConstChange <$> arbitrary <*> arbitrary
    , TimeChange <$> smallMap
    ]

instance Arbitrary i => Arbitrary (SequenceOp i) where
  arbitrary = oneof [SoAfter <$> arbitrary, return SoAbsent]

instance Arbitrary SubOp where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary DataErrorIndex where
  arbitrary = oneof
    [ return GlobalError
    , PathError <$> arbitrary
    , TimePointError <$> arbitrary <*> arbitrary
    ]

instance Arbitrary SubErrorIndex where
  arbitrary = oneof
    [ NamespaceSubError <$> arbitrary
    , PathSubError <$> arbitrary <*> arbitrary
    , TypeSubError <$> arbitrary <*> arbitrary
    , PostTypeSubError <$> arbitrary <*> arbitrary
    ]


creates :: Gen Creates
creates = smallMapOf arbitrary smallMap

contOps :: Arbitrary a => Gen (ContOps a)
contOps = smallMapOf arbitrary smallMap


instance Arbitrary TrpDigest where
  arbitrary = Trpd <$> arbitrary <*> smallMap <*> smallMap
    <*> arbitrary <*> contOps <*> arbitrary
  shrink (Trpd ns pds ds dat cops _errs) =
    [Trpd ns pds' ds' dat' cops' mempty
    | (pds', ds', dat', cops') <- shrink (pds, ds, dat, cops)]

instance Arbitrary TrprDigest where
  arbitrary = Trprd <$> arbitrary


instance Arbitrary TrcSubDigest where
  arbitrary = Trcsd <$> smallMap <*> smallMap <*> smallMap
  shrink (Trcsd pts ts dat) =
    [Trcsd pts' ts' dat' | (pts', ts', dat') <- shrink (pts, ts, dat)]

instance Arbitrary TrcUpdateDigest where
  arbitrary = Trcud <$> arbitrary <*> arbitrary <*> creates
    <*> contOps
  shrink (Trcud ns dat crs cops) =
    [Trcud ns dat' crs' cops'
    | (dat', crs', cops') <- shrink (dat, crs, cops)]


instance Arbitrary FrpDigest where
  arbitrary = Frpd <$> arbitrary <*> arbitrary <*> creates <*> contOps
  shrink (Frpd ns dat crs cops) =
    [Frpd ns dat' crs' cops'
    | (dat', crs', cops') <- shrink (dat, crs, cops)]

instance Arbitrary FrpErrorDigest where
  arbitrary = Frped <$> arbitrary
  shrink (Frped mol) = Frped <$> shrink mol

instance Arbitrary FrcRootDigest where
  arbitrary = Frcrd <$> smallMap
  shrink (Frcrd m) = Frcrd <$> shrink m

instance Arbitrary FrcSubDigest where
  arbitrary = Frcsd <$> smallMapOf arbitrary (smallListOf arbitrary)
    <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (Frcsd e pt t d) =
    [Frcsd e' pt' t' d' | (e', pt', t', d') <- shrink (e, pt, t, d)]

instance Arbitrary FrcUpdateDigest where
  arbitrary = Frcud <$> arbitrary <*> smallMap <*> smallMap
    <*> smallMap <*> arbitrary <*> contOps
    <*> arbitrary
  shrink (Frcud ns pt ty tas d cops _errs) =
    [Frcud ns pt' ty' tas' d' cops' mempty
    | (pt', ty', tas', d', cops') <- shrink (pt, ty, tas, d, cops)]


instance Arbitrary SomeTrDigest where
  arbitrary = oneof
    [ SomeTrDigest <$> arbitrary @TrpDigest
    , SomeTrDigest <$> arbitrary @TrprDigest
    , SomeTrDigest <$> arbitrary @TrcSubDigest
    , SomeTrDigest <$> arbitrary @TrcUpdateDigest
    ]
  shrink (SomeTrDigest d) = case d of
    Trpd {} -> SomeTrDigest <$> shrink d
    Trprd {} -> SomeTrDigest <$> shrink d
    Trcsd {} -> SomeTrDigest <$> shrink d
    Trcud {} -> SomeTrDigest <$> shrink d

instance Arbitrary SomeFrDigest where
  arbitrary = oneof
    [ SomeFrDigest <$> arbitrary @FrpDigest
    , SomeFrDigest <$> arbitrary @FrpErrorDigest
    , SomeFrDigest <$> arbitrary @FrcRootDigest
    , SomeFrDigest <$> arbitrary @FrcSubDigest
    , SomeFrDigest <$> arbitrary @FrcUpdateDigest
    ]
  shrink (SomeFrDigest d) = case d of
    Frpd {} -> SomeFrDigest <$> shrink d
    Frped {} -> SomeFrDigest <$> shrink d
    Frcrd {} -> SomeFrDigest <$> shrink d
    Frcsd {} -> SomeFrDigest <$> shrink d
    Frcud {} -> SomeFrDigest <$> shrink d

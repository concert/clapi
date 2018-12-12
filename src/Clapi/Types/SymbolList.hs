{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , KindSignatures
  , LambdaCase
  , MultiParamTypeClasses
  , PolyKinds
  , RankNTypes
  , StandaloneDeriving
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module Clapi.Types.SymbolList
  ( withKnownSymbol
  , SymbolList(..), SomeSymbolList(..), withSymbolList
  , cons, cons_, singleton, singleton_
  , toStrings, toStrings_, fromStrings, fromType, withSymbolListFromStrings

  , Length, length, (!!)
  , Reverse, reverse

  , PrefixProof(..), isPrefixOf, IsPrefixOf
  , prefixProvesLte
  ) where

import Prelude hiding (length, (!!), reverse)

import Data.Proxy
import Data.Type.Equality (TestEquality(..), (:~:)(..))

import GHC.TypeLits
  (Symbol, KnownSymbol, SomeSymbol(..), someSymbolVal, symbolVal, sameSymbol)

import Clapi.Types.Nat
  (Nat(..), SNat(..), type (<), proofLT, (:<:)(..), (:<=:)(..))


withKnownSymbol :: (forall s. KnownSymbol s => Proxy s -> r) -> String -> r
withKnownSymbol f s = case someSymbolVal s of
  SomeSymbol p -> f p


data SymbolList (ss :: [Symbol]) where
  SlEmpty :: SymbolList '[]
  SlCons :: KnownSymbol s => Proxy s -> SymbolList ss -> SymbolList ('(:) s ss)

instance Show (SymbolList sl) where
  showsPrec p sl = showParen (p >= 11) $
      showString "fromStrings "
    . showsPrec p (toStrings sl)

instance Eq (SymbolList ss) where
  _ == _ = True

instance Ord (SymbolList ss) where
  compare _ _ = EQ

instance TestEquality SymbolList where
  testEquality SlEmpty SlEmpty = Just Refl
  testEquality (SlCons p1 sl1) (SlCons p2 sl2) =
    case (sameSymbol p1 p2, testEquality sl1 sl2) of
      (Just Refl, Just Refl) -> Just Refl
      _ -> Nothing
  testEquality _ _ = Nothing


data SomeSymbolList where
  SomeSymbolList :: SymbolList ss -> SomeSymbolList

withSymbolList :: (forall ss. SymbolList ss -> r) -> SomeSymbolList -> r
withSymbolList f (SomeSymbolList sl) = f sl

deriving instance Show SomeSymbolList

instance Eq SomeSymbolList where
  SomeSymbolList sl1 == SomeSymbolList sl2 = case testEquality sl1 sl2 of
    Just Refl -> sl1 == sl2
    Nothing -> False

instance Ord SomeSymbolList where
  SomeSymbolList sl1 `compare` SomeSymbolList sl2 = go sl1 sl2
    where
      go :: SymbolList ss1 -> SymbolList ss2 -> Ordering
      go SlEmpty SlEmpty = EQ
      go SlEmpty (SlCons _ _) = LT
      go (SlCons _ _) SlEmpty = GT
      go (SlCons p1 sl1') (SlCons p2 sl2') =
        compare (symbolVal p1) (symbolVal p2) <> go sl1' sl2'


cons :: String -> SymbolList ss -> SomeSymbolList
cons s sl = withKnownSymbol (\p -> SomeSymbolList $ SlCons p sl) s

cons_ :: String -> SomeSymbolList -> SomeSymbolList
cons_ s (SomeSymbolList sl) = cons s sl

singleton :: KnownSymbol s => Proxy s -> SymbolList '[s]
singleton p = SlCons p SlEmpty

singleton_ :: String -> SomeSymbolList
singleton_ = withKnownSymbol (SomeSymbolList . singleton)

toStrings :: SymbolList ss -> [String]
toStrings = \case
  SlEmpty -> []
  SlCons p sl -> symbolVal p : toStrings sl

toStrings_ :: SomeSymbolList -> [String]
toStrings_ = withSymbolList toStrings

withSymbolListFromStrings
  :: forall r. (forall ss. SymbolList ss -> r) -> [String] -> r
withSymbolListFromStrings f = go SlEmpty
  where
    go :: SymbolList ss -> [String] -> r
    go acc [] = f $ reverse acc  -- FIXME: reverse is wasteful
    go acc (s : ss) = case someSymbolVal s of
      SomeSymbol p -> go (SlCons p acc) ss

fromStrings :: [String] -> SomeSymbolList
fromStrings = withSymbolListFromStrings SomeSymbolList


class SlFromType ss where
  fromType :: SymbolList ss
instance SlFromType '[] where
  fromType = SlEmpty
instance (KnownSymbol s, SlFromType ss) => SlFromType ('(:) s ss) where
  fromType = SlCons (Proxy @s) fromType

--                        -------- Reverse --------
type family Reverse (as :: [k]) :: [k] where
  Reverse as = Reverse' '[] as

-- Undecidable:
type family Reverse' (acc :: [k]) (as :: [k]) :: [k] where
  Reverse' acc '[] = acc
  Reverse' acc ('(:) a as) = Reverse' (a : acc) as

reverse :: SymbolList ss -> SymbolList (Reverse ss)
reverse = go SlEmpty
  where
    go :: SymbolList ss1 -> SymbolList ss2 -> SymbolList (Reverse' ss1 ss2)
    go acc SlEmpty = acc
    go acc (SlCons p sl) = go (SlCons p acc) sl


--                        -------- Length --------
type family Length (as :: [k]) :: Nat where
  Length '[] = 'Zero
  Length ('(:) a as) = 'Succ (Length as)

length :: SymbolList ss -> SNat (Length ss)
length = \case
  SlEmpty -> SZero
  SlCons _ sl -> SSucc $ length sl

(!!) :: n < Length ss => SymbolList ss -> SNat n -> SomeSymbol
(!!) = get proofLT
  where
    get :: n :<: Length ss -> SymbolList ss -> SNat n -> SomeSymbol
    get LT1 (SlCons p _) SZero = SomeSymbol p
    get (LT2 subProof) (SlCons _ sl) (SSucc n) = get subProof sl n


--                        -------- Prefix --------
data PrefixProof (as1 :: [k]) (as2 :: [k]) where
  PO1 :: PrefixProof '[] as2
  PO2 :: a1 :~: a2 -> PrefixProof as1 as2
      -> PrefixProof ('(:) a1 as1) ('(:) a2 as2)

isPrefixOf :: SymbolList ss1 -> SymbolList ss2 -> Maybe (PrefixProof ss1 ss2)
SlEmpty `isPrefixOf` _ = Just PO1
SlCons {} `isPrefixOf` SlEmpty = Nothing
SlCons p1 sl1 `isPrefixOf` SlCons p2 sl2 =
  case (sameSymbol p1 p2, sl1 `isPrefixOf` sl2) of
    (Just Refl, Just subProof) -> Just $ PO2 Refl subProof
    _ -> Nothing

class (ss1 :: [Symbol]) `IsPrefixOf` (ss2 :: [Symbol]) where
  prefixProof :: PrefixProof ss1 ss2
instance '[] `IsPrefixOf` ss2 where
  prefixProof = PO1
instance (s1 ~ s2, sl1 `IsPrefixOf` sl2) =>
    ('(:) s1 sl1) `IsPrefixOf` ('(:) s2 sl2) where
  prefixProof = PO2 Refl prefixProof

prefixProvesLte :: PrefixProof ss1 ss2 -> (Length ss1) :<=: (Length ss2)
prefixProvesLte = \case
  PO1 -> LTE1
  PO2 Refl subProof -> LTE2 $ prefixProvesLte subProof

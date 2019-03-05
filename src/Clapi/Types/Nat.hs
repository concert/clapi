{-# LANGUAGE
    DataKinds
  , FlexibleInstances
  , GADTs
  , KindSignatures
  , MultiParamTypeClasses
  , RankNTypes
  , StandaloneDeriving
  , TypeOperators
#-}

module Clapi.Types.Nat where

import Data.Constraint (Dict(..))
import Data.Word

-- | A regular datatype for the Peano numbers that we can lift type level
data Nat = Zero | Succ Nat

-- | Singleton witnesses for type level Nats
data SNat (n :: Nat) where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)

toWord32 :: SNat n -> Word32
toWord32 = \case
  SZero -> 0
  SSucc m -> 1 + toWord32 m

instance Show (SNat n) where
  show = show . toWord32

instance Eq (SNat n) where
  _ == _ = True

instance Ord (SNat n) where
  compare _ _ = EQ

withSNatFromWord32 :: forall r. (forall n. SNat n -> r) -> Word32 -> r
withSNatFromWord32 f = go SZero
  where
    go :: SNat n -> Word32 -> r
    go n 0 = f n
    go n m = go (SSucc n) (m - 1)

data SomeSNat where
  SomeSNat :: SNat n -> SomeSNat

withSNat :: (forall n. SNat n -> r) -> SomeSNat -> r
withSNat f (SomeSNat n) = f n

deriving instance Show SomeSNat

instance Eq SomeSNat where
  (SomeSNat m) == (SomeSNat n) = toWord32 m == toWord32 n

instance Ord SomeSNat where
  compare (SomeSNat m) (SomeSNat n) = compare (toWord32 m) (toWord32 n)

fromWord32 :: Word32 -> SomeSNat
fromWord32 =  withSNatFromWord32 SomeSNat


data (m :: Nat) :<: (n :: Nat) where
  LT1 :: 'Zero :<: 'Succ n
  LT2 :: m :<: n -> ('Succ m) :<: ('Succ n)
deriving instance Show (m :<: n)

(%<) :: SNat m -> SNat n -> Maybe (Dict (m < n))
_ %< SZero = Nothing
SZero %< SSucc _ = Just Dict
SSucc m %< SSucc n = fmap (\Dict -> Dict) $ m %< n

class (m :: Nat) < (n :: Nat) where
  proofLT :: m :<: n
instance 'Zero < 'Succ n where
  proofLT = LT1
instance m < n => 'Succ m < 'Succ n where
  proofLT = LT2 proofLT

ltDict :: m :<: n -> Dict (m < n)
ltDict LT1 = Dict
ltDict (LT2 subProof) = case ltDict subProof of Dict -> Dict


data (m :: Nat) :<=: (n :: Nat) where
  LTE1 :: 'Zero :<=: n
  LTE2 :: m :<=: n -> 'Succ m :<=: 'Succ n
deriving instance Show (m :<=: n)

(%<=) :: SNat m -> SNat n -> Maybe (Dict (m <= n))
SZero %<= _ = Just Dict
SSucc _ %<= SZero = Nothing
SSucc m %<= SSucc n = fmap (\Dict -> Dict) $ m %<= n

class (m :: Nat) <= (n :: Nat) where
  proofLTE :: m :<=: n
instance 'Zero <= n where
  proofLTE = LTE1
instance (m <= n) => ('Succ m <= 'Succ n) where
  proofLTE = LTE2 proofLTE

lteDict :: m :<=: n -> Dict (m <= n)
lteDict LTE1 = Dict
lteDict (LTE2 subProof) = case lteDict subProof of Dict -> Dict


upcastLt :: m :<: n -> m :<=: n
upcastLt = \case
  LT1 -> LTE1
  LT2 subProof -> LTE2 $ upcastLt subProof

extendLt :: m :<: n1 -> n1 :<=: n2 -> m :<: n2
extendLt LT1 (LTE2 _) = LT1
extendLt (LT2 ltSubProof) (LTE2 lteSubProof) =
  LT2 $ extendLt ltSubProof lteSubProof

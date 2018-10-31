{-# LANGUAGE
    AllowAmbiguousTypes
  , ConstraintKinds
  , DataKinds
  , ExistentialQuantification
  , FlexibleInstances
  , GADTs
  , InstanceSigs
  , LambdaCase
  , MultiParamTypeClasses
  , PolyKinds
  , Rank2Types
  , StandaloneDeriving
  , TemplateHaskell
  , TypeFamilies
  , TypeFamilyDependencies
  , TypeInType
  , TypeOperators
#-}

module Clapi.Types.Wire
  -- ( -- Wireable
  -- -- , WireValue(..), castWireValue
  -- -- , (<|$|>), (<|*|>)
  -- -- , cast'
  -- WireType(..) --, wireValueWireType, withWtProxy, withWvValue

  -- , NewWireValue(..), NewWireable(..), SomeWireable(..), unwrapNwv
  -- ) where
  where

import Prelude hiding (fail)

import Control.Monad.Fail (MonadFail(..))
import Data.Bifunctor (bimap)
import Data.Constraint (Dict(..), mapDict, ins)
import Data.Int
import Data.Kind (Type, Constraint)
import Data.Text (Text)
import Data.Word
import Data.Type.Equality (TestEquality(..), (:~:))
import Data.Typeable
import Data.Maybe (fromJust)

import GHC.TypeLits (ErrorMessage((:<>:)))
import qualified GHC.TypeLits as Lits

import Clapi.Serialisation.Base (Encodable)
import Clapi.Types.Base (Time(..))
import Clapi.Types.WireTH (mkWithWtProxy)
import Clapi.Util (proxyF, proxyF3)


-- data WireType
--   = WtTime
--   | WtWord32 | WtWord64
--   | WtInt32 | WtInt64
--   | WtFloat | WtDouble
--   | WtString
--   | WtList WireType
--   | WtMaybe WireType
--   | WtPair WireType WireType
--   deriving (Show, Eq, Ord)

data WireType a where
  WtTime :: WireType Time
  WtWord32 :: WireType Word32
  WtList :: WireType x -> WireType [x]
  WtPair :: WireType x -> WireType y -> WireType (x, y)

deriving instance Eq (WireType a)
-- deriving instance Ord (WireType a)

instance TestEquality WireType where
  testEquality WtTime WtTime = Just Refl
  testEquality WtWord32 WtWord32 = Just Refl
  testEquality (WtList wt1) (WtList wt2) = liftRefl <$> testEquality wt1 wt2
  testEquality (WtPair wt1x wt1y) (WtPair wt2x wt2y) =
    pairRefl <$> testEquality wt1x wt2x <*> testEquality wt1y wt2y
  testEquality _ _ = Nothing

liftRefl :: a :~: b -> f a :~: f b
liftRefl Refl = Refl

pairRefl :: a :~: b -> c :~: d -> (a, c) :~: (b, d)
pairRefl Refl Refl = Refl

getEq :: WireType a -> Dict (Eq a)
getEq = \case
  WtTime -> Dict
  WtWord32 -> Dict
  WtList wt -> mapDict ins $ getEq wt
  WtPair wt1 wt2 -> pairDict (getEq wt1) (getEq wt2)

pairDict :: Dict (Eq a) -> Dict (Eq b) -> Dict (Eq (a, b))
pairDict Dict Dict = Dict

data WireValue where
  WireValue :: WireType a -> a -> WireValue

instance Eq WireValue where
  WireValue wt1 a1 == WireValue wt2 a2 = case testEquality wt1 wt2 of
    Nothing -> False
    Just Refl -> case getEq wt1 of
      Dict -> a1 == a2

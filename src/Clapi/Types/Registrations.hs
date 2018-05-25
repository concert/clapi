{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE
    DataKinds
  , UndecidableInstances
  , GADTs
  , KindSignatures
#-}

module Clapi.Types.Registrations where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Monoid ((<>))

import Clapi.Types.TypeName (TypeName, TypeNamespace(..))
import Clapi.Types.Path (Path)

data Registrations = Registrations
  { rTreeTy :: Set (TypeName 'TnTree)
  , rCreateTy :: Set (TypeName 'TnCreate)
  , rValueTy :: Set (TypeName 'TnValue)
  , rData :: Set Path
  } deriving (Eq, Show)

instance Monoid Registrations where
    mempty = Registrations mempty mempty mempty mempty
    mappend = union

union :: Registrations -> Registrations -> Registrations
union (Registrations t0 c0 v0 d0) (Registrations t1 c1 v1 d1) =
    Registrations (t0 <> t1) (c0 <> c1) (v0 <> v1) (d0 <> d1)

data R2 a where
  R2 :: Functor a =>
    { rtt :: a (TypeName 'TnTree)
    , rct :: a (TypeName 'TnCreate)
    } -> R2 a

instance (Functor a, Monoid (a (TypeName 'TnTree)), Monoid (a (TypeName 'TnCreate))) => Monoid (R2 a) where
    mempty = R2 mempty mempty
    mappend _ _ = mempty

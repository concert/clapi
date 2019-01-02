{-# LANGUAGE TemplateHaskell #-}

module Clapi.Internal.Valuespace
  ( Valuespace(..)
  , DefMap, TypeAssignmentMap, Referer, Referee, Xrefs
  , vsTree, vsPostDefs, vsTyDefs, vsRootDefName, vsRootEditability
  , vsTyAssns, vsXrefs
  ) where

import Control.Lens (makeLenses)
import Data.Map (Map)
import Data.Set (Set)
import Data.Tagged (Tagged)

import Data.Map.Dependencies (Dependencies)

import Clapi.Tree (RoseTree)
import Clapi.Types.Definitions
  (SomeDefinition, PostDefinition, Editability, DefName)
import Clapi.Types.Digests (TpId)
import Clapi.Types.Path (Seg, Path)
import Clapi.Types.Wire (SomeWireValue)

type DefMap def = Map (Tagged def Seg) def
type TypeAssignmentMap = Dependencies Path DefName
type Referer = Path
type Referee = Path
type Xrefs = Map Referee (Map Referer (Maybe (Set TpId)))

data Valuespace = Valuespace
  { _vsTree :: RoseTree [SomeWireValue]
  , _vsPostDefs :: DefMap PostDefinition
  , _vsTyDefs :: DefMap SomeDefinition
  , _vsRootDefName :: DefName
  , _vsRootEditability :: Editability
  -- These are just caches that help us do reverse lookups:
  , _vsTyAssns :: TypeAssignmentMap
  , _vsXrefs :: Xrefs
  } deriving Show

makeLenses ''Valuespace

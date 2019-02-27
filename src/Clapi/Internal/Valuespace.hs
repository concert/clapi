{-# LANGUAGE
    TemplateHaskell
  , TypeFamilies
#-}

module Clapi.Internal.Valuespace
  ( Valuespace(..)
  , DefMap, DefKey, TypeAssignmentMap, Referer, Referee, Xrefs
  , vsTree, vsPostDefs, vsTyDefs, vsRootDefName, vsRootEditability
  , vsTyAssns, vsTac

  , EPS
  ) where

import Control.Lens (makeLenses)
import Data.Map (Map)
import Data.Set (Set)

import Data.Map.Dependencies (Dependencies)

import Clapi.Tree (RoseTree)
import Clapi.Types.Definitions
  (SomeDefinition, PostDefinition, Editability)
import Clapi.Types.Base (TpId)
import Clapi.Types.Name (DataName, DefName, PostDefName, Placeholder)
import Clapi.Types.Path (Path)
import Clapi.Types.Wire (SomeWireValue)
-- FIXME: These modules are becoming a bit of a messy tangle
import qualified Clapi.Valuespace.Xrefs as VsXrefs

type family DefKey a
type instance DefKey SomeDefinition = DefName
type instance DefKey PostDefinition = PostDefName

type DefMap def = Map (DefKey def) def
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
  , _vsTac :: VsXrefs.TypeAssertionCache
  } deriving Show

makeLenses ''Valuespace


type EPS = Either Placeholder DataName

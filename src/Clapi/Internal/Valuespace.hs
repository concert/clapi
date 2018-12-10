module Clapi.Internal.Valuespace
  ( Valuespace(..)
  , DefMap, TypeAssignmentMap, Referer, Referee, Xrefs
  ) where

import Data.Map (Map)
import Data.Set (Set)
import Data.Tagged (Tagged)

import Data.Map.Dependencies (Dependencies)

import Clapi.Tree (RoseTree)
import Clapi.Types.Definitions
  (SomeDefinition, PostDefinition, Editable, DefName)
import Clapi.Types.Digests (TpId)
import Clapi.Types.Path (Seg, Path)
import Clapi.Types.Wire (WireValue)

type DefMap def = Map (Tagged def Seg) def
type TypeAssignmentMap = Dependencies Path DefName
type Referer = Path
type Referee = Path
type Xrefs = Map Referee (Map Referer (Maybe (Set TpId)))

data Valuespace = Valuespace
  { vsTree :: RoseTree [WireValue]
  , vsPostDefs :: DefMap PostDefinition
  , vsTyDefs :: DefMap SomeDefinition
  , vsTyAssns :: TypeAssignmentMap
  , vsXrefs :: Xrefs
  , vsRootEditable :: Editable
  } deriving Show

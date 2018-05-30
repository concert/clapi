{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Rank2Types #-}

module Clapi.Types.Definitions where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Data.Map (Map)
import Data.Text (Text)

import Data.Maybe.Clapi (note)

import Clapi.Types.AssocList (AssocList, unAssocList)
import Clapi.Types.Base (InterpolationLimit(..))
import Clapi.Types.Path (Seg, TypeName)
import Clapi.Types.Tree (TreeType(..))

data Liberty = Cannot | May | Must deriving (Show, Eq, Enum, Bounded)
data Required = Required | Optional deriving (Show, Eq, Enum, Bounded)

data MetaType = Tuple | Struct | Array deriving (Show, Eq, Enum, Bounded)

class OfMetaType metaType where
  metaType :: metaType -> MetaType
  childTypeFor :: Seg -> metaType -> Maybe TypeName
  childLibertyFor :: MonadFail m => metaType -> Seg -> m Liberty

data PostDefinition = PostDefinition
  { postDefDoc :: Text
  , postDefArgs :: Map Seg (TreeType, Required)
  } deriving (Show, Eq)

data TupleDefinition = TupleDefinition
  { tupDefDoc :: Text
  -- FIXME: this should eventually boil down to a single TreeType (NB remove
  -- names too and just write more docstring) now that we have pairs:
  , tupDefTypes :: AssocList Seg TreeType
  , tupDefInterpLimit :: InterpolationLimit
  } deriving (Show, Eq)

instance OfMetaType TupleDefinition where
  metaType _ = Tuple
  childTypeFor _ _ = Nothing
  childLibertyFor _ _ = fail "Tuples have no children"

data StructDefinition = StructDefinition
  { strDefDoc :: Text
  , strDefTypes :: AssocList Seg (TypeName, Liberty)
  } deriving (Show, Eq)

instance OfMetaType StructDefinition where
  metaType _ = Struct
  childTypeFor seg (StructDefinition _ tyInfo) =
    fst <$> lookup seg (unAssocList tyInfo)
  childLibertyFor (StructDefinition _ tyInfo) seg = note "No such child" $
    snd <$> lookup seg (unAssocList tyInfo)

data ArrayDefinition = ArrayDefinition
  { arrDefDoc :: Text
  , arrDefChildType :: TypeName
  , arrDefChildLiberty :: Liberty
  } deriving (Show, Eq)

instance OfMetaType ArrayDefinition where
  metaType _ = Array
  childTypeFor _ (ArrayDefinition _ tp _) = Just tp
  childLibertyFor (ArrayDefinition _ _ l) _ = return l


data Definition
  = TupleDef TupleDefinition
  | StructDef StructDefinition
  | ArrayDef ArrayDefinition
  deriving (Show, Eq)

tupleDef :: Text -> AssocList Seg TreeType -> InterpolationLimit -> Definition
tupleDef doc types interpl = TupleDef $ TupleDefinition doc types interpl

structDef :: Text -> AssocList Seg (TypeName, Liberty) -> Definition
structDef doc types = StructDef $ StructDefinition doc types

arrayDef :: Text -> TypeName -> Liberty -> Definition
arrayDef doc tn lib = ArrayDef $ ArrayDefinition doc tn lib

defDispatch :: (forall a. OfMetaType a => a -> r) -> Definition -> r
defDispatch f (TupleDef d) = f d
defDispatch f (StructDef d) = f d
defDispatch f (ArrayDef d) = f d

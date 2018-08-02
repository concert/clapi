{-# LANGUAGE Rank2Types #-}

module Clapi.Types.Definitions where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Data.Tagged (Tagged)
import Data.Text (Text)

import Data.Maybe.Clapi (note)

import Clapi.Types.AssocList (AssocList, unAssocList)
import Clapi.Types.Base (InterpolationLimit(..))
import Clapi.Types.Path (Seg, TypeName)
import Clapi.Types.Tree (TreeType(..))

data Editable = Editable | ReadOnly deriving (Show, Eq, Enum, Bounded)

data MetaType = Tuple | Struct | Array deriving (Show, Eq, Enum, Bounded)

class OfMetaType metaType where
  metaType :: metaType -> MetaType
  childTypeFor :: Seg -> metaType -> Maybe (Tagged Definition TypeName)
  childEditableFor :: MonadFail m => metaType -> Seg -> m Editable

data PostDefinition = PostDefinition
  { postDefDoc :: Text
  -- FIXME: We really need to stop treating single values as lists of types,
  -- which makes the "top level" special:
  , postDefArgs :: AssocList Seg [TreeType]
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
  childEditableFor _ _ = fail "Tuples have no children"

data StructDefinition = StructDefinition
  { strDefDoc :: Text
  , strDefTypes :: AssocList Seg (Tagged Definition TypeName, Editable)
  } deriving (Show, Eq)

instance OfMetaType StructDefinition where
  metaType _ = Struct
  childTypeFor seg (StructDefinition _ tyInfo) =
    fst <$> lookup seg (unAssocList tyInfo)
  childEditableFor (StructDefinition _ tyInfo) seg = note "No such child" $
    snd <$> lookup seg (unAssocList tyInfo)

data ArrayDefinition = ArrayDefinition
  { arrDefDoc :: Text
  , arrPostType :: Maybe (Tagged PostDefinition TypeName)
  , arrDefChildType :: Tagged Definition TypeName
  , arrDefChildEditable :: Editable
  } deriving (Show, Eq)

instance OfMetaType ArrayDefinition where
  metaType _ = Array
  childTypeFor _ = Just . arrDefChildType
  childEditableFor ad _ = return $ arrDefChildEditable ad


data Definition
  = TupleDef TupleDefinition
  | StructDef StructDefinition
  | ArrayDef ArrayDefinition
  deriving (Show, Eq)

tupleDef :: Text -> AssocList Seg TreeType -> InterpolationLimit -> Definition
tupleDef doc types interpl = TupleDef $ TupleDefinition doc types interpl

structDef
  :: Text -> AssocList Seg (Tagged Definition TypeName, Editable) -> Definition
structDef doc types = StructDef $ StructDefinition doc types

arrayDef :: Text
  -> Maybe (Tagged PostDefinition TypeName) -> Tagged Definition TypeName
  -> Editable -> Definition
arrayDef doc ptn tn ed = ArrayDef $ ArrayDefinition doc ptn tn ed

defDispatch :: (forall a. OfMetaType a => a -> r) -> Definition -> r
defDispatch f (TupleDef d) = f d
defDispatch f (StructDef d) = f d
defDispatch f (ArrayDef d) = f d

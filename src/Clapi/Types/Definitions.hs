{-# LANGUAGE
    DataKinds
  , GADTs
  , KindSignatures
  , LambdaCase
  , MultiParamTypeClasses
  , RankNTypes
  , StandaloneDeriving
#-}

module Clapi.Types.Definitions where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Data.Foldable (toList)
import Data.Tagged (Tagged)
import Data.Text (Text)
import Data.Type.Equality (TestEquality(..), (:~:)(..))

import Data.Maybe.Clapi (note)

import Clapi.Types.AssocList (AssocList(..))
import qualified Clapi.Types.AssocList as AL
import Clapi.Types.Base (InterpolationLimit, TypeEnumOf(..))
import Clapi.Types.Path (Seg)
import Clapi.Types.Tree (SomeTreeType(..))

data Editability = Editable | ReadOnly deriving (Show, Eq, Enum, Bounded)

data MetaType = Tuple | Struct | Array deriving (Show, Eq, Ord, Enum, Bounded)

type DefName = Tagged SomeDefinition Seg
type PostDefName = Tagged PostDefinition Seg

data PostDefinition = PostDefinition
  { postDefDoc :: Text
  -- FIXME: We really need to stop treating single values as lists of types,
  -- which makes the "top level" special:
  , postDefArgs :: AssocList Seg [SomeTreeType]
  } deriving (Show, Eq)

data Definition (mt :: MetaType) where
  TupleDef ::
    { tupDefDoc :: Text
  -- FIXME: this should eventually boil down to a single TreeType (NB remove
  -- names too and just write more docstring) now that we have pairs:
    , tupDefTys :: AssocList Seg SomeTreeType
    , tupDefILimit :: InterpolationLimit
    } -> Definition 'Tuple
  StructDef ::
    { strDefDoc :: Text
    , strDefChildTys :: AssocList Seg (DefName, Editability)
    } -> Definition 'Struct
  ArrayDef ::
    { arrDefDoc :: Text
    , arrDefPostTy :: Maybe PostDefName
    , arrDefChildTy :: DefName
    , arrDefChildEd :: Editability
    } -> Definition 'Array
deriving instance Show (Definition mt)
deriving instance Eq (Definition mt)

instance TestEquality Definition where
  TupleDef {} `testEquality` TupleDef {} = Just Refl
  StructDef {} `testEquality` StructDef {} = Just Refl
  ArrayDef {} `testEquality` ArrayDef {} = Just Refl
  _ `testEquality` _ = Nothing

data SomeDefinition where
  SomeDefinition :: Definition mt -> SomeDefinition
deriving instance Show SomeDefinition

instance Eq SomeDefinition where
  SomeDefinition d1 == SomeDefinition d2 = case testEquality d1 d2 of
    Just Refl -> d1 == d2
    Nothing -> False

withDefinition :: (forall mt. Definition mt -> r) -> SomeDefinition -> r
withDefinition f (SomeDefinition d) = f d

tupleDef
  :: Text -> AssocList Seg SomeTreeType -> InterpolationLimit -> SomeDefinition
tupleDef doc tys ilimit = SomeDefinition $ TupleDef doc tys ilimit

structDef :: Text -> AssocList Seg (DefName, Editability) -> SomeDefinition
structDef doc tyinfo = SomeDefinition $ StructDef doc tyinfo

arrayDef
  :: Text -> Maybe PostDefName -> DefName -> Editability -> SomeDefinition
arrayDef doc ptn tn ed = SomeDefinition $ ArrayDef doc ptn tn ed

instance TypeEnumOf (Definition mt) MetaType where
  typeEnumOf = \case
    TupleDef {} -> Tuple
    StructDef {} -> Struct
    ArrayDef {} -> Array

instance TypeEnumOf SomeDefinition MetaType where
  typeEnumOf = withDefinition typeEnumOf


refersTo :: Definition mt -> [DefName]
refersTo = \case
  TupleDef {} -> mempty
  StructDef { strDefChildTys = tyInfo } -> fst <$> toList tyInfo
  ArrayDef { arrDefChildTy = dn } -> pure dn

childTypeFor :: Seg -> Definition mt -> Maybe DefName
childTypeFor seg = \case
  TupleDef {} -> Nothing
  StructDef { strDefChildTys = tyinfo } ->
    fst <$> lookup seg (unAssocList tyinfo)
  ArrayDef { arrDefChildTy = tn } -> Just tn

childEditableFor :: MonadFail m => Seg -> Definition mt -> m Editability
childEditableFor seg = \case
  TupleDef {} -> fail "Tuples have no children"
  StructDef { strDefChildTys = tyinfo } -> note "No such child" $
    snd <$> lookup seg (unAssocList tyinfo)
  ArrayDef { arrDefChildEd = ed } -> return ed

getTyInfoForSeg
  :: MonadFail m => Seg -> Definition mt -> m (DefName, Editability)
getTyInfoForSeg childName = \case
  TupleDef {} -> fail "Tuples do not have children"
  StructDef { strDefChildTys = tyInfo } ->
    maybe (fail $ "Invalid struct child") return
    $ AL.lookup childName tyInfo
  ArrayDef { arrDefChildTy = dn, arrDefChildEd = ed } -> return (dn, ed)

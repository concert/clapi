{-# LANGUAGE
    DataKinds
  , GADTs
  , KindSignatures
  , MultiParamTypeClasses
  , RankNTypes
  , StandaloneDeriving
#-}

module Clapi.Types.Definitions where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Type.Equality (TestEquality(..), (:~:)(..))

import Clapi.Types.AssocList (AssocList(..))
import qualified Clapi.Types.AssocList as AL
import Clapi.Types.Base (InterpolationLimit, TypeEnumOf(..))
import Clapi.Types.Name
  ( DataName, DefName, PostDefName, PostArgName, TupMemberName)
import Clapi.Types.Tree (SomeTreeType(..))

data Editability = Editable | ReadOnly deriving (Show, Eq, Enum, Bounded)

data MetaType = Tuple | Struct | Array deriving (Show, Eq, Ord, Enum, Bounded)

data PostDefinition = PostDefinition
  { postDefDoc :: Text
  -- FIXME: We really need to stop treating single values as lists of types,
  -- which makes the "top level" special:
  , postDefArgs :: AssocList PostArgName [SomeTreeType]
  } deriving (Show, Eq)

data Definition (mt :: MetaType) where
  TupleDef ::
    { tupDefDoc :: Text
  -- FIXME: this should eventually boil down to a single TreeType (NB remove
  -- names too and just write more docstring) now that we have pairs:
    , tupDefTys :: AssocList TupMemberName SomeTreeType
    , tupDefILimit :: InterpolationLimit
    } -> Definition 'Tuple
  StructDef ::
    { strDefDoc :: Text
    , strDefChildTys :: AssocList DataName (DefName, Editability)
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
  :: Text -> AssocList TupMemberName SomeTreeType -> InterpolationLimit
  -> SomeDefinition
tupleDef doc tys ilimit = SomeDefinition $ TupleDef doc tys ilimit

structDef
  :: Text -> AssocList DataName (DefName, Editability) -> SomeDefinition
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

childTypeFor :: DataName -> Definition mt -> Maybe DefName
childTypeFor name = \case
  TupleDef {} -> Nothing
  StructDef { strDefChildTys = tyinfo } ->
    fst <$> lookup name (unAssocList tyinfo)
  ArrayDef { arrDefChildTy = tn } -> Just tn


getTyInfoForName
  :: MonadFail m => DataName -> Definition mt -> m (DefName, Editability)
getTyInfoForName childName = \case
  TupleDef {} -> fail "Tuples do not have children"
  StructDef { strDefChildTys = tyInfo } ->
    maybe (fail "Invalid struct child") return
    $ AL.lookup childName tyInfo
  ArrayDef { arrDefChildTy = dn, arrDefChildEd = ed } -> return (dn, ed)

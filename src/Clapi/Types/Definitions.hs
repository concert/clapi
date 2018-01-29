{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Rank2Types #-}

module Clapi.Types.Definitions where

import Prelude hiding (fail)
import Control.Monad (join)
import Control.Monad.Fail (MonadFail(..))
import Data.Text (Text)
import Data.Word

import Data.Maybe.Clapi (note)

import Clapi.TextSerialisation (ttToText, ttFromText)
import Clapi.Types.AssocList (AssocList, unAssocList, alFromZip)
import Clapi.Types.Base (InterpolationLimit(..))
import Clapi.Types.Path
  (Seg, mkSeg, unSeg, TypeName, typeNameToText, typeNameFromText)
import Clapi.Types.Tree (TreeType)
import Clapi.Types.Wire (WireValue(..), (<|$|>), (<|*|>))
import Clapi.Util (strictZip, fmtStrictZipError, safeToEnum)

data Liberty = Cannot | May | Must deriving (Show, Eq, Enum, Bounded)

data MetaType = Tuple | Struct | Array deriving (Show, Eq, Enum, Bounded)

class OfMetaType metaType where
  metaType :: metaType -> MetaType
  toWireValues :: metaType -> [WireValue]
  fromWireValues :: MonadFail m => [WireValue] -> m metaType
  childTypeFor :: metaType -> Seg -> Maybe TypeName
  childLibertyFor :: MonadFail m => metaType -> Seg -> m Liberty

data TupleDefinition = TupleDefinition
  { tupDefDoc :: Text
  , tupDefTypes :: AssocList Seg TreeType
  , tupDefInterpLimit :: InterpolationLimit
  } deriving (Show, Eq)

instance OfMetaType TupleDefinition where
  metaType _ = Tuple
  toWireValues (TupleDefinition d tys il) =
    let
      (names, treeTypes) = unzip $ unAssocList tys
    in
      [ WireValue d
      , WireValue $ unSeg <$> names
      , WireValue $ ttToText <$> treeTypes
      , WireValue @Word8 $ fromIntegral $ fromEnum il
      ]

  fromWireValues [txt, txts0, txts1, w8] =
      join $ mkDef <|$|> txt <|*|> txts0 <|*|> txts1 <|*|> w8
    where
      mkDef
        :: MonadFail m => Text -> [Text] -> [Text] -> Word8
        -> m TupleDefinition
      mkDef d ns ts il = do
        names <- mapM mkSeg ns
        types <- mapM ttFromText ts
        al <- alFromZip names types
        interp <- safeToEnum $ fromIntegral il
        return $ TupleDefinition d al interp
  fromWireValues _ = fail "Wrong number of arguments for tuple def"

  childTypeFor _ _ = Nothing
  childLibertyFor _ _ = fail "Tuples have no children"

data StructDefinition = StructDefinition
  { strDefDoc :: Text
  , strDefTypes :: AssocList Seg (TypeName, Liberty)
  } deriving (Show, Eq)

instance OfMetaType StructDefinition where
  metaType _ = Struct
  toWireValues (StructDefinition d tys) =
    let
      (names, tys') = unzip $ unAssocList tys
      (tps, libs) = unzip tys'
    in
      [ WireValue d
      , WireValue $ unSeg <$> names
      , WireValue $ typeNameToText <$> tps
      , WireValue @[Word8] $ fromIntegral . fromEnum <$> libs
      ]

  fromWireValues [txt, txts0, txts1, w8s] =
      join $ mkDef <|$|> txt <|*|> txts0 <|*|> txts1 <|*|> w8s
    where
      mkDef
        :: MonadFail m => Text -> [Text] -> [Text] -> [Word8]
        -> m StructDefinition
      mkDef d ns tns ls = do
        names <- mapM mkSeg ns
        typeNames <- mapM typeNameFromText tns
        clibs <- mapM (safeToEnum . fromIntegral) ls
        StructDefinition d <$>
          (fmtStrictZipError "type names" "liberties"
             (strictZip typeNames clibs) >>= alFromZip names)
  fromWireValues _ = fail "Wrong number of arguments for struct def"

  childTypeFor (StructDefinition _ tyInfo) seg =
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
  toWireValues (ArrayDefinition d ct cl) =
    [ WireValue d
    , WireValue $ typeNameToText ct
    , WireValue @Word8 $ fromIntegral $ fromEnum cl
    ]

  fromWireValues [doc, ty, liberty] =
      join $ mkDef <|$|> doc <|*|> ty <|*|> liberty
    where
      mkDef :: MonadFail m => Text -> Text -> Word8 -> m ArrayDefinition
      mkDef d tn l = ArrayDefinition d
        <$> typeNameFromText tn <*> safeToEnum (fromIntegral l)
  fromWireValues _ = fail "Wrong number of arguments for array def"

  childTypeFor (ArrayDefinition _ tp _) _ = Just tp
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

valuesToDef :: MonadFail m => MetaType -> [WireValue] -> m Definition
valuesToDef Tuple wvs = TupleDef <$> fromWireValues wvs
valuesToDef Struct wvs = StructDef <$> fromWireValues wvs
valuesToDef Array wvs = ArrayDef <$> fromWireValues wvs

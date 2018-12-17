{-# LANGUAGE
    GADTs
  , TemplateHaskell
#-}

module Clapi.Internal.TreeType where

import Data.Constraint (Dict(..))
import Data.Int
import Data.Word
import Data.Set (Set)
import Data.Text (Text)
import Language.Haskell.TH

import Clapi.Types.Base (Time(..))
import Clapi.Types.EnumVal (EnumVal)
import Clapi.Types.Path (Seg, Path)
import Clapi.Types.SymbolList (SymbolList)
import Clapi.Types.UniqList (UniqList)


data Bounds a = Bounds (Maybe a) (Maybe a) deriving (Show, Eq, Ord)


-- | A value-level witness for any type that can be held in the CLAPI tree
data TreeType a where
  TtTime :: TreeType Time
  -- This constructor is kinda the odd-one-out, because its constraint makes it
  -- through to the type:
  TtEnum :: SymbolList ss -> TreeType (EnumVal ss)
  TtWord32 :: Bounds Word32 -> TreeType Word32
  TtWord64 :: Bounds Word64 -> TreeType Word64
  TtInt32 :: Bounds Int32 -> TreeType Int32
  TtInt64 :: Bounds Int64 -> TreeType Int64
  TtFloat :: Bounds Float -> TreeType Float
  TtDouble :: Bounds Double -> TreeType Double
  TtString :: Text -> TreeType Text
  -- FIXME: kinda want this to be `TtRef DefName`, but that creates an import
  -- loop:
  TtRef :: Seg -> TreeType Path
  TtList :: TreeType a -> TreeType [a]
  TtSet :: TreeType a -> TreeType (Set a)
  TtOrdSet :: TreeType a -> TreeType (UniqList a)
  TtMaybe :: TreeType a -> TreeType (Maybe a)
  TtPair :: TreeType a -> TreeType b -> TreeType (a, b)


mkGetTtConstraint :: String -> Type -> Q [Dec]
mkGetTtConstraint newFnName constrCon =
  let
    fn = mkName newFnName
    a = mkName "a"
    tt1 = mkName "tt1"; tt2 = mkName "tt2"
    arrow :: Type -> Type -> Type
    arrow t1 t2 = AppT (AppT ArrowT t1) t2
    dictP = ConP 'Dict []
    dictE = ConE 'Dict
    simpleCase n i = Match (ConP n $ replicate i WildP) (NormalB dictE) []
    recurse tt = AppE (VarE fn) (VarE tt)
    recursiveCase n = Match
      (ConP n [VarP tt1])
      (NormalB $ CaseE (recurse tt1)
        [Match dictP (NormalB dictE) []])
      []
  in
    return
    [ SigD fn $
      (AppT (ConT ''TreeType) (VarT a))
      `arrow`
      (AppT (ConT ''Dict) (AppT constrCon (VarT a)))
    , FunD fn $
      [ Clause
         []
         ( NormalB $ LamCaseE
           [ simpleCase 'TtTime 0
           , simpleCase 'TtEnum 1
           , simpleCase 'TtWord32 1
           , simpleCase 'TtWord64 1
           , simpleCase 'TtInt32 1
           , simpleCase 'TtInt64 1
           , simpleCase 'TtFloat 1
           , simpleCase 'TtDouble 1
           , simpleCase 'TtString 1
           , simpleCase 'TtRef 1
           , recursiveCase 'TtList
           , recursiveCase 'TtSet
           , recursiveCase 'TtOrdSet
           , recursiveCase 'TtMaybe
           , Match
             (ConP 'TtPair [VarP tt1, VarP tt2])
             ( NormalB $ CaseE
               (TupE [recurse tt1, recurse tt2])
               [ Match
                 (TupP [dictP, dictP])
                 (NormalB dictE)
                 []
               ]
             )
             []
           ]
         )
         []
      ]
    ]

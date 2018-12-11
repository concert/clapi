{-# LANGUAGE
    GADTs
  , TemplateHaskell
#-}

module Clapi.Internal.Wire where

import Data.Constraint (Dict(..))
import Data.Int
import Data.Word
import Data.Text (Text)
import Language.Haskell.TH

import Clapi.Types.Base (Time(..))


data WireType a where
  WtTime :: WireType Time
  WtWord32 :: WireType Word32
  WtWord64 :: WireType Word64
  WtInt32 :: WireType Int32
  WtInt64 :: WireType Int64
  WtFloat :: WireType Float
  WtDouble :: WireType Double
  WtString :: WireType Text
  WtList :: WireType x -> WireType [x]
  WtMaybe :: WireType x -> WireType (Maybe x)
  WtPair :: WireType x -> WireType y -> WireType (x, y)


mkGetWtConstraint :: String -> Type -> Q [Dec]
mkGetWtConstraint newFnName constrCon =
  let
    fn = mkName newFnName
    a = mkName "a"
    wt1 = mkName "wt1"; wt2 = mkName "wt2"
    arrow :: Type -> Type -> Type
    arrow t1 t2 = AppT (AppT ArrowT t1) t2
    simpleCase n = Match
      (ConP n [])
      (NormalB dictE)
      []
    recurse wt = AppE (VarE fn) (VarE wt)
    dictP = ConP 'Dict []
    dictE = ConE 'Dict
    recursiveCase n = Match
      (ConP n [VarP wt1])
      (NormalB $ CaseE (recurse wt1)
        [Match dictP (NormalB dictE) []])
      []
  in
    return
    [ SigD (mkName newFnName) $
        (AppT (ConT ''WireType) (VarT a))
        `arrow`
        (AppT (ConT ''Dict) (AppT constrCon (VarT a)))
    , FunD (mkName newFnName) $
      [ Clause
          []
          ( NormalB $ LamCaseE
            [ simpleCase 'WtTime
            , simpleCase 'WtWord32
            , simpleCase 'WtWord64
            , simpleCase 'WtInt32
            , simpleCase 'WtInt64
            , simpleCase 'WtFloat
            , simpleCase 'WtDouble
            , simpleCase 'WtString
            , recursiveCase 'WtList
            , recursiveCase 'WtMaybe
            , Match
                (ConP 'WtPair [VarP wt1, VarP wt2])
                ( NormalB $ CaseE
                    (TupE [recurse wt1, recurse wt2])
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

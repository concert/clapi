{-# LANGUAGE
    DeriveLift
  , StandaloneDeriving
  , TypeApplications
#-}
module Clapi.Types.WireTH where

import Data.Proxy

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))

deriving instance Lift (Proxy a)

mkWithWtProxy :: String -> [Name] -> Q [Dec]
mkWithWtProxy newFnName preds =
  let
    wt = mkName "wt"; f = mkName "f"; a = mkName "a"; r = mkName "r"
    wt1 = mkName "wt1"; wt2 = mkName "wt2"; p1 = mkName "p1"; p2 = mkName "p2"
    withWtProxy wtE fE = AppE (AppE (VarE $ mkName newFnName) wtE) fE
    proxy tName = AppTypeE (ConE $ mkName "Proxy") (ConT $ mkName tName)
    proxyF e = AppE (VarE $ mkName "proxyF") e
    proxyF3 e1 e2 e3 = AppE (AppE (AppE (VarE $ mkName "proxyF3")  e1) e2) e3
    arrow :: Type -> Type -> Type
    arrow t1 t2 = AppT (AppT ArrowT t1) t2
    dot e1 e2 = UInfixE e1 (VarE $ mkName ".") e2
    simpleCase :: String -> Match
    simpleCase s = simpleCase' s s
    simpleCase' :: String -> String -> Match
    simpleCase' wtName tName = Match
      (ConP (mkName $ "Wt" ++ wtName) [])
      (NormalB $ AppE (VarE f) (proxy tName))
      []
  in
    return $
      [ -- WireType -> (forall a. (pred1 a, pred2 a...) => Proxy a -> r) -> r
        SigD (mkName newFnName) $
          (ConT $ mkName "WireType")
          `arrow`
          ((ForallT [PlainTV a] ((flip AppT $ VarT a) . ConT <$> preds) $
            (AppT (ConT $ mkName "Proxy") (VarT a))
            `arrow`
            VarT r)
          `arrow`
          VarT r)
      , FunD (mkName newFnName)
        [ Clause
            [VarP wt, VarP f]
            (NormalB $ CaseE (VarE wt)
              [ simpleCase "Time"
              , simpleCase "Word8"
              , simpleCase "Word32"
              , simpleCase "Word64"
              , simpleCase "Int32"
              , simpleCase "Int64"
              , simpleCase "Float"
              , simpleCase "Double"
              , simpleCase' "String" "Text"
              , Match
                  (ConP (mkName "WtList") [VarP wt1])
                  (NormalB $ withWtProxy
                    (VarE wt1)
                    (VarE f `dot` proxyF (proxy "[]")))
                  []
              , Match
                  (ConP (mkName "WtMaybe") [VarP wt1])
                  (NormalB $ withWtProxy
                    (VarE wt1)
                    (VarE f `dot` proxyF (proxy "Maybe")))
                  []
              , Match
                  (ConP (mkName "WtPair") [VarP wt1, VarP wt2])
                  (NormalB $ withWtProxy (VarE wt1) (LamE [VarP p1] $
                    withWtProxy (VarE wt2) (LamE [VarP p2] $
                      AppE (VarE f) (proxyF3 (proxy "(,)") (VarE p1) (VarE p2))
                    ))
                  )
                  []
              ]
            )
            []
        ]
      ]

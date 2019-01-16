{-# LANGUAGE
    LambdaCase
  , OverloadedStrings
  , QuasiQuotes
#-}
module Valuespace2Spec where

import Test.Hspec

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (StateT, liftIO, evalStateT)
import Data.Either (isRight)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tagged (Tagged(..))
import Data.Word
import Text.Printf (printf)

import Data.Map.Mol (Mol)
import qualified Data.Map.Mol as Mol

import Clapi.TH (segq, pathq)
import Clapi.Types.AssocList (alSingleton, alFromList)
import Clapi.Types.Base
  (InterpolationType(..), Interpolation(..), Time(..))
import Clapi.Types.Definitions
  (Editability(..), DefName, SomeDefinition, tupleDef, structDef, arrayDef)
import Clapi.Types.Digests
  ( TpId, DefOp(..), DataErrorIndex(..), DataChange(..), TimeSeriesDataOp(..)
  , TrDigest(..), trpdEmpty
  , FrDigest(..), frcudEmpty)
import Clapi.Types.Path (Namespace(..), Path, pattern Root, pattern (:/))
import Clapi.Types.Tree (SomeTreeType, bounds, unbounded, ttWord32, ttRef)
import Clapi.Types.Wire (someWv, WireType(..))
import Clapi.Valuespace2

import Helpers (timeLimit)
import Instances ()


spec :: Spec
spec =
  describe "processTrpd" $ do
    it "validates baseValuespace with no changes" $ go $
       processTrpd (trpdEmpty ns) >>= (`succeedsWith` frcudEmpty ns)
    it "validates constant data changes" $ go $ do
       res <- processTrpd $ (trpdEmpty ns)
         { trpdDefs = Map.singleton rootDn $ OpDefine $ boundedW32Tup 3 5
         , trpdData = alSingleton Root $ ConstChange Nothing
            [someWv WtWord32 4]
         }
       liftIO $ succeedsWith res $ (frcudEmpty ns)
         { frcudDefs = Map.singleton rootDn $ OpDefine $ boundedW32Tup 3 5
         , frcudData = alSingleton Root $ ConstChange Nothing
             [someWv WtWord32 4]
         }
       res' <- processTrpd $ (trpdEmpty ns)
         { trpdData = alSingleton Root $ ConstChange Nothing
            [someWv WtWord32 7]
         }
       liftIO $ errorsOn Root res'
    describe "time series changes" $
      let
        mkTpSet i =
          (i, (Nothing, OpSet (Time 0 i) [someWv WtWord32 i] IConstant))
        setup = do
          res <- processTrpd $ (trpdEmpty ns)
            { trpdDefs = Map.singleton rootDn $ OpDefine w32Ts
            , trpdData = alSingleton Root $ TimeChange $ Map.fromList
               [mkTpSet 0, mkTpSet 1, mkTpSet 2]
            }
          liftIO $ succeedsWith res $ (frcudEmpty ns)
            { frcudDefs = Map.singleton rootDn $ OpDefine w32Ts
            , frcudData = alSingleton Root $ TimeChange $ Map.fromList
               [mkTpSet 0, mkTpSet 1, mkTpSet 2]
            }
      in do
        it "validates data" $ go $ do
          setup
          res <- processTrpd $ (trpdEmpty ns)
            { trpdData = alSingleton Root $ TimeChange $ Map.singleton 1
                (Nothing, OpSet (Time 0 1) [someWv WtWord32 7] IConstant)
            }
          liftIO $ errorsOnTp Root 1 res
        it "validates interpolation" $ go $ do
          setup
          res <- processTrpd $ (trpdEmpty ns)
            { trpdData = alSingleton Root $ TimeChange $ Map.singleton 2
                (Nothing, OpSet (Time 0 2) [someWv WtWord32 2] $ IBezier 0 0)
            }
          liftIO $ errorsOnTp Root 2 res
        it "catches overlapping time points" $ go $ do
          setup
          res <- processTrpd $ (trpdEmpty ns)
            { trpdData = alSingleton Root $ TimeChange $ Map.singleton 0
                (Nothing, OpSet (Time 0 1) [someWv WtWord32 2] $ IConstant)
            }
          liftIO $ errorsOnTp Root 0 res
    it "(re)validates on type changes" $ go $ do
      let trpd = (trpdEmpty ns)
            {trpdDefs = Map.singleton rootDn $ OpDefine w32Tup}
      res <- processTrpd trpd
      errorsOn Root res

      let trpd' = trpd
            {trpdData = alSingleton Root $
              ConstChange Nothing [someWv WtWord64 1]}
      res' <- processTrpd trpd'
      errorsOn Root res'

      let change = alSingleton Root $ ConstChange Nothing [someWv WtWord32 1]
      let trpd'' = trpd {trpdData = change}
      res'' <- processTrpd trpd''
      succeedsWith res'' $ (frcudEmpty ns)
        { frcudDefs = Map.singleton rootDn $ OpDefine w32Tup
        , frcudData = change
        }
    it "Errors on struct with missing child" $ go $
      let
        w32Tn = Tagged [segq|w32|]
        trpd = (trpdEmpty ns)
          { trpdDefs = Map.fromList
             [ (rootDn, OpDefine $ structDef "foo and bar" $ alFromList
                 [ ([segq|foo|], (w32Tn, ReadOnly))
                 , ([segq|bar|], (w32Tn, ReadOnly))
                 ])
             , (w32Tn, OpDefine w32Tup)
             ]
          }
      in do
        res <- processTrpd trpd
        errorsOn Root res
        errorsOn [pathq|/foo|] res
        errorsOn [pathq|/bar|] res

        res' <- processTrpd $ trpd
          { trpdData = alSingleton [pathq|/foo|] $
              ConstChange Nothing [someWv WtWord32 17] }
        errorsOn Root res'
        -- FIXME: There's a change in behaviour from when the struct node is not
        -- there at all. We should consistently either output either just errors
        -- on the parent path or errors on missing child paths too:
        -- errorsOn [pathq|/bar|] res'

        res'' <- processTrpd $ trpd
          { trpdData = alFromList
            [ ([pathq|/foo|], ConstChange Nothing [someWv WtWord32 17])
            , ([pathq|/bar|], ConstChange Nothing [someWv WtWord32 18])
            ]
          }
        succeeds res''
    describe "Recursive struct definitions" $ let r = [segq|r|] in do
      it "rejected in a direct loop" $ timeLimit 1 $ go $ do
        res <- processTrpd $ (trpdEmpty ns)
          { trpdDefs = Map.singleton rootDn $ OpDefine $
              structDef "Recursive!" $ alSingleton r (rootDn, ReadOnly)
          }
        errors GlobalError res
      it "rejected in an indirect loop" $ timeLimit 1 $ go $ do
        res <- processTrpd $ (trpdEmpty ns)
          { trpdDefs = Map.fromList
              [ (rootDn, OpDefine $ structDef "rec1" $
                  alSingleton r (Tagged r, ReadOnly))
              , (Tagged r, OpDefine $ structDef "rec2" $
                  alSingleton r (rootDn, ReadOnly))
              ]
          }
        errors GlobalError res
      it "rejected below an array" $ timeLimit 1 $ go $ do
        res <- processTrpd $ (trpdEmpty ns)
         { trpdDefs = Map.fromList
             [ (rootDn, OpDefine $
                 arrayDef "rec below" Nothing (Tagged r) ReadOnly)
             , (Tagged r, OpDefine $ structDef "recursive!" $
                 alSingleton r (Tagged r, ReadOnly))
             ]
         }
        errors GlobalError res
      it "accepted if mediated by an array" $ timeLimit 1 $ go $ do
        res <- processTrpd $ (trpdEmpty ns)
          { trpdDefs = Map.fromList
              [ (rootDn, OpDefine $ structDef "rec1" $
                  alSingleton r (Tagged r, ReadOnly))
              , (Tagged r, OpDefine $
                  arrayDef "rec2" Nothing rootDn ReadOnly)
              ]
          , trpdData = alFromList
              [
              ]
          }
        succeeds res
    describe "Cross reference validation" $
      let
        referer = [segq|referer|]
        referee = [segq|referee|]
        setup = do
          res <- processTrpd $ (trpdEmpty ns)
            { trpdDefs = Map.fromList
                [ (rootDn, OpDefine $ structDef "root" $ alFromList
                    [ ([segq|referer|], (Tagged referer, ReadOnly))
                    , ([segq|referee|], (Tagged referee, ReadOnly))
                    ])
                , (Tagged referer, OpDefine $ tupleDef "referer"
                    (alSingleton [segq|r|] $ ttRef referee)
                    Nothing)
                , (Tagged referee, OpDefine w32Tup)
                ]
            , trpdData = alFromList
                [ (Root :/ referee, ConstChange Nothing [someWv WtWord32 11])
                , (Root :/ referer, ConstChange Nothing [someWv WtString "/referee"])
                ]
            }
          succeeds res
      in do
        it "catches referer type changes" $ go $ do
          setup
        it "catches referee type changes" $ go $ return ()
        it "does not hold on to old references" $ go $ return ()
  where
    go :: StateT Valuespace IO a -> IO a
    go = flip evalStateT bvs

errors
  :: (Show x1, MonadIO m)
  => DataErrorIndex -> Either (Mol DataErrorIndex x1) x2 -> m ()
errors idx = liftIO . \case
  Left m -> unless (idx `Set.member` Mol.keysSet m) $ expectationFailure $
    printf "No error on %s. Errors: %s" (show idx) (show m)
  Right _ -> expectationFailure "Did not error"

errorsOn
  :: (Show x1, MonadIO m) => Path -> Either (Mol DataErrorIndex x1) x2 -> m ()
errorsOn p = errors (PathError p)

errorsOnTp
  :: (Show x1, MonadIO m)
  => Path -> TpId -> Either (Mol DataErrorIndex x1) x2 -> m ()
errorsOnTp p tpid = errors (TimePointError p tpid)

succeeds :: (Show e, Show a, MonadIO m) => Either e a -> m ()
succeeds ea = liftIO $ ea `shouldSatisfy` isRight

succeedsWith
  :: (Eq a, Eq e, Show e, Show a, MonadIO m) => Either e a -> a -> m ()
succeedsWith ea a = liftIO $ ea `shouldBe` Right a

ns :: Namespace
ns = Namespace [segq|test_ns|]

rootDn :: DefName
rootDn = Tagged [segq|root|]

bvs :: Valuespace
bvs = baseValuespace rootDn ReadOnly

w32Ty :: SomeTreeType
w32Ty = ttWord32 unbounded

boundedW32 :: Word32 -> Word32 -> SomeTreeType
boundedW32 lo hi = either error ttWord32 $ bounds (Just lo) (Just hi)

w32Tup :: SomeDefinition
w32Tup = tupleDef "w32" (alSingleton [segq|val|] w32Ty) Nothing

boundedW32Tup :: Word32 -> Word32 -> SomeDefinition
boundedW32Tup lo hi = tupleDef "bounded w32"
  (alSingleton [segq|val|] $ boundedW32 lo hi)
  Nothing

w32Ts :: SomeDefinition
w32Ts = tupleDef "w32Ts" (alSingleton [segq|val|] $ boundedW32 0 5) (Just ItLinear)

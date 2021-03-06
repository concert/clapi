{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , QuasiQuotes
#-}
module ValuespaceSpec where

import Test.Hspec

import Control.Lens (use)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (StateT, liftIO, evalStateT, get)
import Data.Either (isRight)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Word
import Text.Printf (printf)

import Data.Map.Mol (Mol)
import qualified Data.Map.Mol as Mol

import Clapi.TH (n, pathq)
import qualified Clapi.Tree as Tree
import qualified Clapi.Types.AssocList as AL
import Clapi.Types.Base
  (InterpolationType(..), Interpolation(..), TpId, Time(..))
import Clapi.Types.Definitions
  ( Editability(..), SomeDefinition, PostDefinition(..)
  , tupleDef, structDef, arrayDef)
import Clapi.Types.Digests
  ( DefOp(..), DataErrorIndex(..), DataChange(..), TimeSeriesDataOp(..)
  , CreateOp(..)
  , TrDigest(..), trpdEmpty, TrcUpdateDigest, trcudEmpty
  , FrDigest(..), frcudEmpty, FrpDigest, frpdEmpty)
import Clapi.Types.Name (DefName, Namespace)
import Clapi.Types.Path (Path, pattern Root, pattern (:/))
import Clapi.Types.SequenceOps (SequenceOp(..), DependencyError(..))
import Clapi.Types.Tree (SomeTreeType, bounds, unbounded, ttWord32, ttRef)
import Clapi.Types.Wire (someWv, WireType(..))
import Clapi.Valuespace

-- FIXME: this should probably end up at Clapi.Valuespace[2].Internal
import Clapi.Internal.Valuespace (vsTree, vsTyDefs)

import Helpers (timeLimit)
import Instances ()


spec :: Spec
spec =
  let
    referer = [n|referer|]
    referee1 = [n|referee1|]
    referee2 = [n|referee2|]
    xrefSetup = do
      res <- processTrpd $ (trpdEmpty ns)
        { trpdDefs = Map.fromList
            [ (rootDn, OpDefine $ structDef "root" $ AL.fromList
                [ (referer, (referer, Editable))
                , (referee1, (referee1, ReadOnly))
                , (referee2, (referee2, ReadOnly))
                ])
            , (referer, OpDefine $ tupleDef "referer"
                (AL.singleton [n|r|] $ ttRef referee1)
                Nothing)
            , (referee1, OpDefine w32Tup)
            , (referee2, OpDefine w32Tup)
            ]
        , trpdData = AL.fromList
            [ (Root :/ referee1,
                 ConstChange Nothing [someWv WtWord32 11])
            , (Root :/ referee2,
                 ConstChange Nothing [someWv WtWord32 12])
            , (Root :/ referer,
                 ConstChange Nothing [someWv WtString "/referee1"])
            ]
        }
      succeeds res

  in do
    describe "processTrpd" $ do
      it "validates baseValuespace with no changes" $ go $
         processTrpd (trpdEmpty ns) >>= (`succeedsWith` frcudEmpty ns)

      it "validates constant data changes" $ go $ do
         res <- processTrpd $ (trpdEmpty ns)
           { trpdDefs = Map.singleton rootDn $ OpDefine $ boundedW32Tup 3 5
           , trpdData = AL.singleton Root $ ConstChange Nothing
              [someWv WtWord32 4]
           }
         succeedsWith res $ (frcudEmpty ns)
           { frcudDefs = Map.singleton rootDn $ OpDefine $ boundedW32Tup 3 5
           , frcudData = AL.singleton Root $ ConstChange Nothing
               [someWv WtWord32 4]
           }
         res' <- processTrpd $ (trpdEmpty ns)
           { trpdData = AL.singleton Root $ ConstChange Nothing
              [someWv WtWord32 7]
           }
         withErrorsOn
           [ PEValidationError $ DataValidationError "7 is not <= 5" ]
           Root
           res'

      describe "time series changes" $
        let
          mkTpSet i =
            (i, (Nothing, OpSet (Time 0 i) [someWv WtWord32 i] IConstant))
          setup = do
            res <- processTrpd $ (trpdEmpty ns)
              { trpdDefs = Map.singleton rootDn $ OpDefine w32Ts
              , trpdData = AL.singleton Root $ TimeChange $ Map.fromList
                 [mkTpSet 0, mkTpSet 1, mkTpSet 2]
              }
            succeedsWith res $ (frcudEmpty ns)
              { frcudDefs = Map.singleton rootDn $ OpDefine w32Ts
              , frcudData = AL.singleton Root $ TimeChange $ Map.fromList
                 [mkTpSet 0, mkTpSet 1, mkTpSet 2]
              }
        in do
          it "validates data" $ go $ do
            setup
            res <- processTrpd $ (trpdEmpty ns)
              { trpdData = AL.singleton Root $ TimeChange $ Map.singleton 1
                  (Nothing, OpSet (Time 0 1) [someWv WtWord32 7] IConstant)
              }
            withErrorsOnTp
              [ PEValidationError $ DataValidationError "7 is not <= 5" ]
              Root 1 res

          it "validates interpolation" $ go $ do
            setup
            res <- processTrpd $ (trpdEmpty ns)
              { trpdData = AL.singleton Root $ TimeChange $ Map.singleton 2
                  (Nothing, OpSet (Time 0 2) [someWv WtWord32 2] $ IBezier 0 0)
              }
            withErrorsOnTp
              [ PEValidationError $ BadInterpolationType ItBezier (Just ItLinear)]
              Root 2 res

          it "catches overlapping time points" $ go $ do
            setup
            res <- processTrpd $ (trpdEmpty ns)
              { trpdData = AL.singleton Root $ TimeChange $ Map.singleton 0
                  (Nothing, OpSet (Time 0 1) [someWv WtWord32 2] $ IConstant)
              }
            withErrorsOnTp
              [PEErrorString "Duplicate k1"]
              Root 0 res

      it "(re)validates data on type changes" $ go $ do
        let trpd = (trpdEmpty ns)
              {trpdDefs = Map.singleton rootDn $ OpDefine w32Tup}
        res <- processTrpd trpd
        withErrorsOn
          [PEStructuralError UnexpectedNodeType] Root res

        let trpd' = trpd
              {trpdData = AL.singleton Root $
                ConstChange Nothing [someWv WtWord64 1]}
        res' <- processTrpd trpd'
        withErrorsOn
          [ PEValidationError $
              DataValidationError "Type mismatch: Cannot produce word32 from WtWord64"
          ]
          Root res'

        let change = AL.singleton Root $ ConstChange Nothing [someWv WtWord32 1]
        let trpd'' = trpd {trpdData = change}
        res'' <- processTrpd trpd''
        succeedsWith res'' $ (frcudEmpty ns)
          { frcudDefs = Map.singleton rootDn $ OpDefine w32Tup
          , frcudData = change
          }

      describe "Child resolution on type changes" $
        let
          foo = [n|foo|]
          common = [n|common|]
          structOnly = [n|structOnly|]
          arrayOnly = [n|arrayOnly|]
          w32Dn = [n|w32|] :: DefName

          setupStruct = do
            res <- processTrpd $ (trpdEmpty ns)
              { trpdDefs = Map.fromList
                 [ (rootDn, OpDefine $ structDef "some struct" $ AL.fromList
                     [ (common, (w32Dn, ReadOnly))
                     , (structOnly, (w32Dn, ReadOnly))
                     ])
                 , (w32Dn, OpDefine w32Tup)
                 ]
              , trpdData = AL.fromList
                  [ ( Root :/ common, ConstChange Nothing [someWv WtWord32 0])
                  , ( Root :/ structOnly
                    , ConstChange Nothing [someWv WtWord32 1])
                  ]
              }
            succeeds res

          setupArray = do
            res <- processTrpd $ (trpdEmpty ns)
              { trpdDefs = Map.fromList
                  [ (rootDn, OpDefine $
                      arrayDef "some array" Nothing w32Dn ReadOnly)
                  , (w32Dn, OpDefine w32Tup)
                  ]
              , trpdData = AL.fromList
                  [ (Root :/ common, ConstChange Nothing [someWv WtWord32 0])
                  , (Root :/ arrayOnly, ConstChange Nothing [someWv WtWord32 0])
                  ]
              }
            succeeds res
        in do
          it "struct -> struct" $ go $ do
            setupStruct
            res <- processTrpd $ (trpdEmpty ns)
              { trpdDefs = Map.singleton rootDn $
                  OpDefine $ structDef "new struct" $ AL.fromList
                    [ (foo, (w32Dn, ReadOnly))
                    , (common, (w32Dn, ReadOnly))
                    ]
              , trpdData = AL.singleton (Root :/ foo) $
                  ConstChange Nothing [someWv WtWord32 2]
              }
            succeeds res
            rootChildrenShouldBe [foo, common]

          it "struct -> array" $ go $ do
            setupStruct
            res <- processTrpd $ (trpdEmpty ns)
              { trpdDefs = Map.singleton rootDn $
                  OpDefine $ arrayDef "new array" Nothing w32Dn ReadOnly
              }
            succeeds res
            rootChildrenShouldBe [common, structOnly]
            -- FIXME: need to check that non-compliant types are validated
            -- out...

          it "array -> struct" $ go $ do
            setupArray
            res <- processTrpd $ (trpdEmpty ns)
              { trpdDefs = Map.singleton rootDn $
                  OpDefine $ structDef "new struct" $ AL.fromList
                    [ (foo, (w32Dn, ReadOnly))
                    , (common, (w32Dn, ReadOnly))
                    ]
              , trpdData = AL.singleton (Root :/ foo) $
                  ConstChange Nothing [someWv WtWord32 2]
              }
            succeeds res
            rootChildrenShouldBe [foo, common]
            -- FIXME: need to check that non-compliant existing children are
            -- validated

      it "handle removing a type definition (unused)" $ go $
        let
          superfluousDn = [n|superfluous|] :: DefName
        in do
          res <- processTrpd $ (trpdEmpty ns)
            { trpdDefs = Map.singleton superfluousDn $ OpDefine w32Tup
            }
          succeeds res

          res' <- processTrpd $ (trpdEmpty ns)
            { trpdDefs = Map.singleton superfluousDn OpUndefine
            }
          succeeds res'

          defs <- use vsTyDefs
          liftIO $ Map.lookup superfluousDn defs `shouldBe` Nothing

      it "handle removing a type definition (uses removed at same time)" $ go $
        let
          foo = [n|foo|]
          w32Dn = [n|w32|] :: DefName
        in do
          res <- processTrpd $ (trpdEmpty ns)
            { trpdDefs = Map.fromList
                [ (rootDn, OpDefine $ structDef "some struct" $ AL.singleton foo
                    (w32Dn, ReadOnly))
                , (w32Dn, OpDefine w32Tup)
                ]
            , trpdData = AL.singleton (Root :/ foo) $
                ConstChange Nothing [someWv WtWord32 0]
            }
          succeeds res

          res' <- processTrpd $ (trpdEmpty ns)
            { trpdDefs = Map.fromList
                [ (w32Dn, OpUndefine)
                , (rootDn, OpDefine $ structDef "empty struct" mempty)
                ]
            }
          succeeds res'

      it "catches in use type when undefined" $ go $
        let
          foo = [n|foo|]
          w32Dn = [n|w32|] :: DefName
        in do
          res <- processTrpd $ (trpdEmpty ns)
            { trpdDefs = Map.fromList
                [ (rootDn, OpDefine $ structDef "some struct" $ AL.singleton foo
                    (w32Dn, ReadOnly))
                , (w32Dn, OpDefine w32Tup)
                ]
            , trpdData = AL.singleton (Root :/ foo) $
                ConstChange Nothing [someWv WtWord32 0]
            }
          succeeds res

          res' <- processTrpd $ (trpdEmpty ns)
            { trpdDefs = Map.singleton w32Dn OpUndefine
            }
          withErrors
            GlobalError [ PEAccessError $ DefNotFound [n|w32|]]
            res'

      it "catches non-existant node type in array definition" $ go $ do
        res <- processTrpd $ (trpdEmpty ns)
          { trpdDefs = Map.singleton rootDn $ OpDefine $ arrayDef
              "naughty array" Nothing [n|nope|] ReadOnly
          }
        withErrors GlobalError [PEAccessError $ DefNotFound [n|nope|]] res

      it "catches non-existant post type in array definition" $ go $ do
        res <- processTrpd $ (trpdEmpty ns)
          { trpdDefs = Map.fromList
              [ (rootDn, OpDefine $ arrayDef "naughty array" (Just [n|nope|])
                  [n|w32Dn|] ReadOnly)
              , ([n|w32Dn|], OpDefine w32Tup)
              ]
          }
        withErrors GlobalError [PEAccessError $ PostDefNotFound [n|nope|]] res

      it "errors on struct with missing child" $ go $
        let
          w32Tn = [n|w32|] :: DefName
          trpd = (trpdEmpty ns)
            { trpdDefs = Map.fromList
               [ (rootDn, OpDefine $ structDef "foo and bar" $ AL.fromList
                   [ ([n|foo|], (w32Tn, ReadOnly))
                   , ([n|bar|], (w32Tn, ReadOnly))
                   ])
               , (w32Tn, OpDefine w32Tup)
               ]
            }
        in do
          res <- processTrpd trpd
          noErrorsOn Root res
          withErrorsOn
            [PEAccessError NodeNotFound] [pathq|/foo|] res
          withErrorsOn
            [PEAccessError NodeNotFound] [pathq|/bar|] res

          res' <- processTrpd $ trpd
            { trpdData = AL.singleton [pathq|/foo|] $
                ConstChange Nothing [someWv WtWord32 17] }
          noErrorsOn Root res'
          noErrorsOn [pathq|/foo|] res'
          withErrorsOn
            [PEAccessError NodeNotFound] [pathq|/bar|] res

          res'' <- processTrpd $ trpd
            { trpdData = AL.fromList
              [ ([pathq|/foo|], ConstChange Nothing [someWv WtWord32 17])
              , ([pathq|/bar|], ConstChange Nothing [someWv WtWord32 18])
              ]
            }
          succeeds res''

      it "errors with extra data" $ go $ do
        res <- processTrpd $ (trpdEmpty ns)
          { trpdData = AL.singleton [pathq|/foo|] $ ConstChange Nothing []
          }
        withErrorsOn
          [PEErrorString "Invalid struct child"] [pathq|/foo|] res

      describe "Container ordering" $
        let
          foo = [n|foo|]
          bar = [n|bar|]
          w32Dn = [n|w32Dn|] :: DefName

          arraySetup = do
            res <- processTrpd $ (trpdEmpty ns)
              { trpdDefs = Map.fromList
                  [ ( rootDn
                    , OpDefine $ arrayDef "some array" Nothing w32Dn ReadOnly)
                  , ( w32Dn, OpDefine w32Tup)
                  ]
              , trpdData = AL.fromList
                  [ (Root :/ foo, ConstChange Nothing [someWv WtWord32 0])
                  , (Root :/ bar, ConstChange Nothing [someWv WtWord32 1])
                  ]
              }
            succeeds res
            rootChildrenShouldBe [foo, bar]
        in do
          it "forbids struct reordering" $ go $ do
            res <- processTrpd $ (trpdEmpty ns)
              { trpdDefs = Map.fromList
                  [ (rootDn, OpDefine $ structDef "some struct" $ AL.fromList
                      [ (foo, (w32Dn, ReadOnly))
                      , (bar, (w32Dn, ReadOnly))
                      ]
                    )
                  , (w32Dn, OpDefine w32Tup)
                  ]
              , trpdData = AL.fromList
                  [ (Root :/ foo, ConstChange Nothing [someWv WtWord32 0])
                  , (Root :/ bar, ConstChange Nothing [someWv WtWord32 1])
                  ]
              }
            succeeds res

            res' <- processTrpd $ (trpdEmpty ns)
              {  trpdContOps = Map.singleton Root $ Map.singleton foo
                   (Nothing, SoAfter $ Just bar)
              }
            withErrorsOn
              [PEStructuralError SeqOpsOnNonArray] Root res'

          it "handles array reordering" $ go $ do
            arraySetup
            res <- processTrpd $ (trpdEmpty ns)
              { trpdContOps = Map.singleton Root $ Map.singleton foo
                  (Nothing, SoAfter $ Just bar)
              }
            succeeds res
            rootChildrenShouldBe [bar, foo]

          it "catches missing reorder targets" $ go $ do
            arraySetup
            res <- processTrpd $ (trpdEmpty ns)
              { trpdContOps = Map.singleton Root $ Map.singleton foo
                  (Nothing, SoAfter $ Just [n|tosh|])
              }
            withErrorsOn
              [PEErrorString "Preceeding element not found for move"] Root res

          it "catches circular reorder target references" $ go $ do
            arraySetup
            res <- processTrpd $ (trpdEmpty ns)
              { trpdContOps = Map.singleton Root $ Map.fromList
                  [ (foo, (Nothing, SoAfter $ Just bar))
                  , (bar, (Nothing, SoAfter $ Just foo))
                  ]
              }
            withErrorsOn
              [ PEDependencyError $
                  CyclicReferences
                    [[ ([n|foo|], Just [n|bar|])
                     , ([n|bar|], Just [n|foo|])
                    ]]
              ] Root res

      describe "Recursive struct definitions" $ let r = [n|r|] in do
        it "rejected in a direct loop" $ timeLimit 1 $ go $ do
          res <- processTrpd $ (trpdEmpty ns)
            { trpdDefs = Map.singleton rootDn $ OpDefine $
                structDef "Recursive!" $ AL.singleton r (rootDn, ReadOnly)
            }
          withErrors GlobalError
            [CircularStructDefinitions [[n|root|], [n|root|]]] res

        it "rejected in an indirect loop" $ timeLimit 1 $ go $ do
          res <- processTrpd $ (trpdEmpty ns)
            { trpdDefs = Map.fromList
                [ (rootDn, OpDefine $ structDef "rec1" $
                    AL.singleton r (r, ReadOnly))
                , (r, OpDefine $ structDef "rec2" $
                    AL.singleton r (rootDn, ReadOnly))
                ]
            }
          withErrors GlobalError
            [CircularStructDefinitions [[n|root|], r, [n|root|]]] res

        it "rejected below an array" $ timeLimit 1 $ go $ do
          res <- processTrpd $ (trpdEmpty ns)
           { trpdDefs = Map.fromList
               [ (rootDn, OpDefine $
                   arrayDef "rec below" Nothing r ReadOnly)
               , (r, OpDefine $ structDef "recursive!" $
                   AL.singleton r (r, ReadOnly))
               ]
           }
          withErrors GlobalError
            [CircularStructDefinitions [r, r]] res

        it "accepted if mediated by an array" $ timeLimit 1 $ go $ do
          res <- processTrpd $ (trpdEmpty ns)
            { trpdDefs = Map.fromList
                [ (rootDn, OpDefine $ structDef "rec1" $
                    AL.singleton r (r, ReadOnly))
                , (r, OpDefine $ arrayDef "rec2" Nothing rootDn ReadOnly)
                ]
            , trpdData = AL.fromList
                [
                ]
            }
          succeeds res

      describe "Cross reference validation (constant data)" $ do
        -- FIXME: we need to do all this again for time series ;-)
        it "catches reference to invalid paths" $ go $ do
          xrefSetup
          res <- processTrpd $ (trpdEmpty ns)
            { trpdData = AL.singleton (Root :/ referer) $
                ConstChange Nothing [someWv WtString "/bad"]
            }
          withErrorsOn
            [PEErrorString "Invalid struct child"] [pathq|/referer|] res

        it "catches references to invalid types" $ go $ do
          xrefSetup
          res <- processTrpd $ (trpdEmpty ns)
            { trpdData = AL.singleton (Root :/ referer) $
                ConstChange Nothing [someWv WtString "/referee2"]
            }
          withErrorsOn
            [PEValidationError $ XRefError referee1 referee2]
            [pathq|/referer|] res

        it "catches referer type change" $ go $
          let
            trpd = (trpdEmpty ns)
              { trpdDefs = Map.singleton referer $
                  OpDefine $ tupleDef "referer"
                    (AL.singleton [n|r|] $ ttRef referee2)
                    Nothing
              }
          in do
            xrefSetup
            res <- processTrpd trpd
            withErrorsOn
              [PEValidationError $ XRefError referee2 referee1]
              [pathq|/referer|] res

            res' <- processTrpd $ trpd
              { trpdData = AL.singleton (Root :/ referer) $
                  ConstChange Nothing [someWv WtString "/referee2"]
              }
            succeeds res'

        it "catches referee type changes" $ go $
          let
            defs = Map.singleton rootDn $
                  OpDefine $ structDef "root" $ AL.fromList
                    [ (referer, (referer, ReadOnly))
                    , (referee1, (referee2, ReadOnly))
                    , (referee2, (referee2, ReadOnly))
                    ]
            trpd = (trpdEmpty ns) { trpdDefs = defs }
          in do
            xrefSetup
            res <- processTrpd trpd
            withErrorsOn
              [ PEValidationError $ XRefError referee1 referee2
              , PEValidationError $ XRefError referee1 referee2
              ]
              [pathq|/referer|] res

            res' <- processTrpd trpd
              { trpdDefs = defs <> (Map.singleton referer $
                  OpDefine $ tupleDef "referer"
                    (AL.singleton [n|r|] $ ttRef referee2)
                    Nothing)
              }
            succeeds res'

      describe "Cross reference validation (time series)" $ do
        it "catches reference to invalid paths" $ pending
        it "catches references to invalid types" $ pending
        it "catches referer type change" $ pending
        it "catches referee type changes" $ pending

    describe "processTrcud" $
      let
        basicStructSetup = do
          res <- processTrpd $ (trpdEmpty ns)
            { trpdDefs = Map.fromList
                [ (rootDn, OpDefine $ structDef "root" $ AL.fromList
                    [ ([n|myWord|], ([n|w32|], Editable))
                    , ([n|otherWord|], ([n|w32|], ReadOnly))
                    ])
                , ([n|w32|], OpDefine w32Tup)
                ]
            , trpdData = AL.fromList
                [ ([pathq|/myWord|], ConstChange Nothing [someWv WtWord32 41])
                , ([pathq|/otherWord|], ConstChange Nothing [someWv WtWord32 0])
                ]
            }
          succeeds res

        basicArraySetup' editability = do
          res <- processTrpd $ (trpdEmpty ns)
            { trpdDefs = Map.fromList
                [ (rootDn, OpDefine $ arrayDef "root"
                    (Just [n|postW32|]) [n|w32|] editability)
                , ([n|w32|], OpDefine w32Tup)
                ]
            , trpdPostDefs = Map.singleton [n|postW32|] $ OpDefine $
                PostDefinition "Post me a Word worthy of Mordor!" $ AL.singleton
                  [n|theWord|] [w32Ty]
            }
          succeeds res

        basicArraySetup = basicArraySetup' Editable

      in do
        it "validates baseValuespace with no changes" $ go $
          processTrcud' (trcudEmpty ns) >>= (`succeedsWith` frpdEmpty ns)

        describe "Write permissions" $ do
          it "prohibits data changes on read-only paths" $ go $ do
            _ <- processTrpd $ (trpdEmpty ns)
              { trpdDefs = Map.singleton rootDn $ OpDefine $ boundedW32Tup 3 5
              , trpdData = AL.singleton Root $ ConstChange Nothing
                  [someWv WtWord32 4]
              }
            res <- processTrcud' $ (trcudEmpty ns)
              { trcudData = AL.singleton Root $ ConstChange Nothing
                  [someWv WtWord32 5]
              }
            withErrorsOn [ReadOnlyEdit] Root res

            basicStructSetup
            res' <- processTrcud' $ (trcudEmpty ns)
              { trcudData = AL.singleton [pathq|/otherWord|] $ ConstChange Nothing
                  [someWv WtWord32 42]
              }
            withErrorsOn [ReadOnlyEdit] [pathq|/otherWord|] res'

            basicArraySetup' ReadOnly
            res'' <- processTrcud' $ (trcudEmpty ns)
              { trcudData = AL.singleton [pathq|/otherWord|] $ ConstChange Nothing
                  [someWv WtWord32 42]
              }
            withErrorsOn [ReadOnlyEdit] [pathq|/otherWord|] res''

          it "probibits child reorderings on read-only paths" $ go $
            let
              foo = [n|foo|]
              bar = [n|bar|]
            in do
              basicArraySetup  -- Root is defined as read-only
              res <- processTrpd $ (trpdEmpty ns)
                { trpdData = AL.fromList
                    [ (Root :/ foo, ConstChange Nothing [someWv WtWord32 0])
                    , (Root :/ bar, ConstChange Nothing [someWv WtWord32 1])
                    ]
                }
              succeeds res

              res' <- processTrcud' $ (trcudEmpty ns)
                { trcudContOps = Map.singleton Root $ Map.singleton (Right foo)
                    (Nothing, SoAfter $ Just $ Right bar)
                }
              withErrorsOn [ReadOnlySeqOps] Root res'

          it "prohibits data changes on non-existant array paths" $ go $ do
            -- Clients can't create array elements without posting!
            basicArraySetup
            res <- processTrcud' $ (trcudEmpty ns)
              { trcudData = AL.singleton [pathq|/newPath|] $ ConstChange Nothing
                  [someWv WtWord32 42]
              }
            withErrorsOn [CEAccessError NodeNotFound] [pathq|/newPath|] res

        it "validates constant data changes" $ go $ do
          basicStructSetup
          res <- processTrcud' $ (trcudEmpty ns)
            { trcudData = AL.singleton [pathq|/myWord|] $ ConstChange Nothing
                [someWv WtInt32 42]
            }
          withErrorsOn
            [ CEValidationError (
                DataValidationError
                  "Type mismatch: Cannot produce word32 from WtInt32")]
            [pathq|/myWord|] res

          res' <- processTrcud' $ (trcudEmpty ns)
            { trcudData = AL.singleton [pathq|/myWord|] $ ConstChange Nothing
                [someWv WtWord32 42]
            }
          succeeds res'

        describe "time series changes" $ do
          it "validates data" $ pending
          it "validates interpolation" $ pending
          it "catches overlapping time points" $ pending

        describe "Create validation" $ do
          it "forbids creates on non-arrays" $ go $ do
            basicStructSetup
            res <- processTrcud' $ (trcudEmpty ns)
              { trcudCreates = Map.singleton Root mempty
              }
            withErrorsOn [CreatesNotSupported] Root res

          it "catches bad create arguments" $ go $
            let
              doCreate vals = processTrcud' $ (trcudEmpty ns)
                { trcudCreates = Map.singleton Root $
                    Map.singleton [n|new|]
                    (Nothing, OpCreate vals)
                }
            in do
              basicArraySetup
              doCreate [] >>= withErrorsOn
                [ CEValidationError (
                    DataValidationError
                      "Mismatched numbers of post def arg types (1) and list of wire values (0)")]
                Root
              doCreate [[]] >>= withErrorsOn
                [ CEValidationError (
                    DataValidationError
                      "Mismatched numbers of types (1) and values (0)")]
                Root
              doCreate [[someWv WtString "bad type"]] >>= withErrorsOn
                [ CEValidationError (
                    DataValidationError
                      "Type mismatch: Cannot produce word32 from WtString")]
                Root
              doCreate [[someWv WtWord32 0]] >>= succeeds

        it "errors with extra data (struct)" $ go $ do
          res <- processTrcud' $ (trcudEmpty ns)
            { trcudData = AL.singleton [pathq|/bad|] $ ConstChange Nothing
                [someWv WtString "Irrelevant"]
            }
          liftIO $ res `shouldBe`
            Left (Mol.singleton (PathError [pathq|/bad|]) $ CEErrorString "Invalid struct child")

        it "errors with extra data (array)" $ go $ do
          basicArraySetup
          res <- processTrcud' $ (trcudEmpty ns)
            { trcudData = AL.singleton [pathq|/bad|] $ ConstChange Nothing
                [someWv WtWord32 0]  -- Good data for array
            }
          withErrorsOn
            [CEAccessError NodeNotFound]
            [pathq|/bad|] res

        it "errors on struct reordering" $
          flip evalStateT (baseValuespace rootDn Editable) $ do
            basicStructSetup
            res <- processTrcud' $ (trcudEmpty ns)
              { trcudContOps = Map.singleton Root
                  $ Map.singleton (Right [n|myWord|])
                  (Nothing, SoAfter $ Just $ Right [n|otherWord|])
              }
            liftIO $ res `shouldBe` Left (
              Mol.singleton (PathError Root) $
              CEStructuralError SeqOpsOnNonArray)

        describe "Array reordering" $
          let
            editableArraySetup = do
              res <- processTrpd $ (trpdEmpty ns)
                { trpdDefs = Map.fromList
                    [ (rootDn, OpDefine $ structDef "some struct" $
                        AL.singleton [n|array|] ([n|earray|], Editable))
                    , ([n|earray|], OpDefine $ arrayDef "editable array"
                        (Just [n|postW32|]) [n|w32|] Editable)
                    , ([n|w32|], OpDefine w32Tup)
                    ]
                , trpdPostDefs = Map.singleton [n|postW32|] $ OpDefine $
                    PostDefinition "Post me a Word" $ AL.singleton
                    [n|theWord|] [w32Ty]
                , trpdData = AL.fromList
                    [ ([pathq|/array/foo|], ConstChange Nothing [someWv WtWord32 0])
                    ]
                }
              succeeds res
           in do
              it "catches missing placeholder targets (naive)" $ go $ do
                editableArraySetup
                res <- processTrcud' $ (trcudEmpty ns)
                  { trcudContOps = Map.singleton [pathq|/array|] $ Map.singleton
                      (Right [n|foo|])
                      (Nothing, SoAfter $ Just $ Left [n|bar|])
                  }
                withErrorsOn
                  [ CESeqOpError $
                      SeqOpTargetMissing (Right [n|foo|]) (Left [n|bar|])
                  ]
                  [pathq|/array|]
                  res
              it "catches missing placeholder targets (other failures)" $ go $ do
                editableArraySetup
                res <- processTrcud' $ (trcudEmpty ns)
                  {  trcudCreates = Map.singleton [pathq|/array|] $
                      Map.singleton [n|bar|]
                        (Nothing, OpCreate [[someWv WtString "wrong type"]])

                  ,  trcudContOps = Map.singleton [pathq|/array|] $ Map.singleton
                      (Right [n|foo|])
                      (Nothing, SoAfter $ Just $ Left [n|bar|])
                  }
                withErrorsOn
                  [ CEValidationError (
                      DataValidationError
                       "Type mismatch: Cannot produce word32 from WtString")
                  ,
                    CESeqOpError $
                      SeqOpTargetMissing (Right [n|foo|]) (Left [n|bar|])
                  ]
                  [pathq|/array|]
                  res
              it "rejects array reorderings referencing missing members" $ go $ do
                editableArraySetup
                res <- processTrcud' $ (trcudEmpty ns)
                  { trcudContOps = Map.singleton [pathq|/array|] $ Map.singleton
                      (Right [n|foo|])
                      (Nothing, SoAfter $ Just $ Right [n|bar|])
                  }
                withErrorsOn
                  [ CESeqOpError $
                      SeqOpTargetMissing (Right [n|foo|]) (Right [n|bar|])
                  ]
                  [pathq|/array|]
                  res
              it "rejects cyclic array reordering targets" $ go $ do
                editableArraySetup
                _ <- processTrpd $ (trpdEmpty ns)
                  { trpdData = AL.fromList
                    [
                      ( [pathq|/array/foo|]
                      , ConstChange Nothing [someWv WtWord32 0])
                    ,
                      ( [pathq|/array/bar|]
                      , ConstChange Nothing [someWv WtWord32 1])
                    ]
                  }
                res <- processTrcud' $ (trcudEmpty ns)
                  { trcudContOps = Map.singleton [pathq|/array|] $ Map.fromList
                    [
                      ( Right [n|bar|]
                      , (Nothing, SoAfter $ Just $ Right [n|foo|]))
                    ,
                      ( Right [n|foo|]
                      , (Nothing, SoAfter $ Just $ Right [n|bar|]))
                    ]
                  }
                withErrorsOn
                  [ CEDependencyError $ CyclicReferences
                    [[ (Right [n|foo|], Just $ Right [n|bar|])
                    ,  (Right [n|bar|], Just $ Right [n|foo|])
                    ]]
                  ]
                  [pathq|/array|]
                  res
              it "rejects duplicate target references" $ go $ do
                editableArraySetup
                _ <- processTrpd $ (trpdEmpty ns)
                  { trpdData = AL.fromList
                    [
                      ( [pathq|/array/foo|]
                      , ConstChange Nothing [someWv WtWord32 0])
                    ,
                      ( [pathq|/array/bar|]
                      , ConstChange Nothing [someWv WtWord32 1])
                    ,
                      ( [pathq|/array/baz|]
                      , ConstChange Nothing [someWv WtWord32 1])
                    ]
                  }
                res <- processTrcud' $ (trcudEmpty ns)
                  { trcudContOps = Map.singleton [pathq|/array|] $ Map.fromList
                    [
                      ( Right [n|baz|]
                      , (Nothing, SoAfter $ Just $ Right [n|foo|]))
                    ,
                      ( Right [n|bar|]
                      , (Nothing, SoAfter $ Just $ Right [n|foo|]))
                    ]
                  }
                withErrorsOn
                  [ CEDependencyError $ DuplicateReferences
                    [(Just $ Right [n|foo|], [Right [n|bar|], Right [n|baz|]])]
                  ]
                  [pathq|/array|] res
              it "accepts valid array reordings" $ go $ do
                editableArraySetup
                res <- processTrcud' $ (trcudEmpty ns)
                  {  trcudCreates = Map.singleton [pathq|/array|] $
                      Map.singleton [n|bar|]
                        (Nothing, OpCreate [[someWv WtWord32 1]])

                  ,  trcudContOps = Map.singleton [pathq|/array|] $ Map.singleton
                      (Right [n|foo|])
                      (Nothing, SoAfter $ Just $ Left [n|bar|])
                  }
                succeeds res

        describe "Cross reference validation" $ do
          it "catches references to invalid types" $ go $ do
            xrefSetup
            res <- processTrcud' $ (trcudEmpty ns)
              { trcudData = AL.singleton (Root :/ referer) $
                  ConstChange Nothing [someWv WtString "/referee2"]
              }
            withErrorsOn
              [CEValidationError $ XRefError [n|referee1|] [n|referee2|]]
              [pathq|/referer|] res

  where
    go :: StateT Valuespace IO a -> IO a
    go = flip evalStateT bvs

    processTrcud'
      :: Monad m
      => TrcUpdateDigest
      -> StateT Valuespace m
         (Either (Mol DataErrorIndex ConsumerError) FrpDigest)
    processTrcud' trcud = get >>= processTrcud trcud >>= \(errs, frpd) ->
      -- FIXME: Might not want to cast this pair to an Either in the end
      return $ if null errs then Right frpd else Left errs
    rootChildrenShouldBe expected = do
      children <- Tree.childNames <$> use vsTree
      liftIO $ children `shouldBe` expected

withErrors
  :: (Eq errs, Show errs, MonadIO m)
  => DataErrorIndex -> [errs] -> Either (Mol DataErrorIndex errs) x2 -> m ()
withErrors idx errs = liftIO . \case
  Left m ->
    if idx `Set.member` Mol.keysSet m then
      let
        actuals = Mol.lookup idx m
      in
        if errs == actuals then return ()
        else expectationFailure $
          printf "Errors differ:\nExpected %s\n Actual %s"
            (show errs) (show actuals)
    else expectationFailure $
      printf "No error on %s. Errors: %s" (show idx) (show m)
  Right _ -> expectationFailure $ printf "Did not error with (%s)" (show idx)

errors
  :: (Show x1, MonadIO m)
  => DataErrorIndex -> Either (Mol DataErrorIndex x1) x2 -> m ()
errors idx = liftIO . \case
  Left m -> unless (idx `Set.member` Mol.keysSet m) $ expectationFailure $
    printf "No error on %s. Errors: %s" (show idx) (show m)
  Right _ -> expectationFailure $ printf "Did not error with (%s)" (show idx)


withErrorsOn
  :: (Eq errs, Show errs, MonadIO m)
  => [errs] -> Path -> Either (Mol DataErrorIndex errs) x2 -> m ()
withErrorsOn e p = withErrors (PathError p) e

withErrorsOnTp
  :: (Eq errs, Show errs, MonadIO m)
  => [errs] -> Path -> TpId -> Either (Mol DataErrorIndex errs) x2 -> m ()
withErrorsOnTp errs p tpid = withErrors (TimePointError p tpid) errs

noErrorsOn
  :: (Show x1, MonadIO m) => Path -> Either (Mol DataErrorIndex x1) x2 -> m ()
noErrorsOn p = liftIO . \case
  Left m -> when (PathError p `Set.member` Mol.keysSet m) $ expectationFailure $
    printf "Unexpected errors on %s (%s)" (show p)
    (show $ Mol.lookup (PathError p) m)
  Right _ -> return ()

succeeds :: (Show e, Show a, MonadIO m) => Either e a -> m ()
succeeds ea = liftIO $ ea `shouldSatisfy` isRight

succeedsWith
  :: (Eq a, Eq e, Show e, Show a, MonadIO m) => Either e a -> a -> m ()
succeedsWith ea a = liftIO $ ea `shouldBe` Right a

ns :: Namespace
ns = [n|test_ns|]

rootDn :: DefName
rootDn = [n|root|]

bvs :: Valuespace
bvs = baseValuespace rootDn ReadOnly

w32Ty :: SomeTreeType
w32Ty = ttWord32 unbounded

boundedW32 :: Word32 -> Word32 -> SomeTreeType
boundedW32 lo hi = either error ttWord32 $ bounds (Just lo) (Just hi)

w32Tup :: SomeDefinition
w32Tup = tupleDef "w32" (AL.singleton [n|val|] w32Ty) Nothing

boundedW32Tup :: Word32 -> Word32 -> SomeDefinition
boundedW32Tup lo hi = tupleDef "bounded w32"
  (AL.singleton [n|val|] $ boundedW32 lo hi)
  Nothing

w32Ts :: SomeDefinition
w32Ts = tupleDef "w32Ts" (AL.singleton [n|val|] $ boundedW32 0 5) (Just ItLinear)

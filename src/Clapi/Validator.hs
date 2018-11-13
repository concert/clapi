{-# LANGUAGE
    ApplicativeDo
  , LambdaCase
  , FlexibleInstances
  , GADTs
  , MultiParamTypeClasses
  , Rank2Types
  , TypeOperators
#-}

module Clapi.Validator where

import Prelude hiding (fail)
import Control.Monad (void, (>=>))
import Control.Monad.Fail (MonadFail(..))
import Data.Constraint (Dict(..), mapDict, ins)
import qualified Data.Set as Set
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Type.Equality ((:~:)(..))
import Data.Word (Word32)
import Text.Regex.PCRE ((=~~))
import Text.Printf (printf, PrintfArg)

import Clapi.Util (safeToEnum, ensureUnique)
import Clapi.Types
  ( WireValue(..), SomeWireValue(..), WireType(..), WireTypeOf, Definition
  , DefName)
import Clapi.Types.EnumVal (enumVal)
import Clapi.Types.Path (Seg, Path)
import qualified Clapi.Types.Path as Path
import Clapi.Types.Tree
  ( Bounds, boundsMin, boundsMax, TreeType(..), SomeTreeType(..)
  , TreeValue(..))
import Clapi.Types.UniqList (mkUniqList)

inBounds :: (Ord a, MonadFail m, PrintfArg a) => Bounds a -> a -> m a
inBounds b n = go (boundsMin b) (boundsMax b)
  where
    success = return n
    gte lo | n >= lo = success
           | otherwise = fail $ printf "%v is not >= %v" n lo
    lte hi | n <= hi = success
           | otherwise = fail $ printf "%v is not <= %v" n hi
    go Nothing Nothing = success
    go (Just lo) Nothing = gte lo
    go Nothing (Just hi) = lte hi
    go (Just lo) (Just hi) = gte lo >> lte hi

checkString :: MonadFail m => Text -> Text -> m Text
checkString r t = maybe
  (fail $ printf "did not match '%s'" r)
  (const $ return t)
  (Text.unpack t =~~ Text.unpack r :: Maybe ())

checkEnum :: MonadFail m => [Seg] -> Word32 -> m Word32
checkEnum ns w = let theMax = fromIntegral $ length ns in
  if w >= theMax
    then fail $ printf "Enum value %v out of range" w
    else return w


typeValid
  :: MonadFail m => WireType a -> TreeType b -> m (a :~: WireTypeOf b)
typeValid WtTime TtTime = return Refl
typeValid WtWord32 (TtWord32 _) = return Refl
typeValid WtWord64 (TtWord64 _) = return Refl
typeValid WtInt32 (TtInt32 _) = return Refl
typeValid WtInt64 (TtInt64 _) = return Refl
typeValid WtFloat (TtFloat _) = return Refl
typeValid WtDouble (TtDouble _) = return Refl
typeValid WtString tt1
  | TtString _ <- tt1 = return Refl
  | TtRef _ <- tt1 = return Refl
typeValid (WtList wt) tt1
  | TtList tt2 <- tt1 = (\Refl -> Refl) <$> typeValid wt tt2
  | TtSet tt2 <- tt1 = (\Refl -> Refl) <$> typeValid wt tt2
  | TtOrdSet tt2 <- tt1 = (\Refl -> Refl) <$> typeValid wt tt2
typeValid (WtMaybe wt) (TtMaybe tt) = (\Refl -> Refl) <$> typeValid wt tt
typeValid (WtPair wt1 wt2) (TtPair tt1 tt2) = do
  Refl <- typeValid wt1 tt1
  Refl <- typeValid wt2 tt2
  return $ Refl
typeValid _ _ = fail "useful message here"

validateValue :: MonadFail m => TreeType a -> WireTypeOf a -> m a
validateValue = \case
    TtTime -> return
    TtEnum sl -> enumVal sl
    TtWord32 b -> inBounds b
    TtWord64 b -> inBounds b
    TtInt32 b -> inBounds b
    TtInt64 b -> inBounds b
    TtFloat b -> inBounds b
    TtDouble b -> inBounds b
    TtString pat -> checkString pat
    TtRef _ -> Path.fromText Path.segP
    TtList tt -> mapM $ validateValue tt
    TtSet tt -> case ordyShowy tt of
      Dict -> mapM (validateValue tt) >=> ensureUnique "items"
              >=> return . Set.fromList
    TtOrdSet tt -> case ordyShowy tt of
      Dict -> mapM (validateValue tt) >=> mkUniqList
    TtMaybe tt -> mapM $ validateValue tt
    TtPair tt1 tt2 -> \(x, y) ->
      (,) <$> validateValue tt1 x <*> validateValue tt2 y
  where
    ordyShowy :: TreeType a -> Dict (Ord a, Show a)
    ordyShowy = \case
      TtTime -> Dict
      TtEnum _ -> Dict
      TtWord32 _ -> Dict
      TtWord64 _ -> Dict
      TtInt32 _ -> Dict
      TtInt64 _ -> Dict
      TtFloat _ -> Dict
      TtDouble _ -> Dict
      TtString _ -> Dict
      TtRef _ -> Dict
      TtList tt -> case ordyShowy tt of Dict -> Dict
      TtSet tt -> case ordyShowy tt of Dict -> Dict
      TtOrdSet tt -> case ordyShowy tt of Dict -> Dict
      TtMaybe tt -> case ordyShowy tt of Dict -> Dict
      TtPair tt1 tt2 -> case (ordyShowy tt1, ordyShowy tt2) of
        (Dict, Dict) -> Dict

validate :: MonadFail m => TreeType a -> WireValue b -> m (TreeValue a)
validate tt (WireValue wt a) = do
  Refl <- typeValid wt tt
  TreeValue tt <$> validateValue tt a

extractTypeAssertions :: TreeType a -> a -> [(DefName, Path)]
extractTypeAssertions = \case
  TtRef s -> \path -> [(Tagged s, path)]
  TtList tt -> foldMap (extractTypeAssertions tt)
  TtSet tt -> foldMap (extractTypeAssertions tt)
  TtOrdSet tt -> foldMap (extractTypeAssertions tt)
  TtMaybe tt -> foldMap (extractTypeAssertions tt)
  TtPair tt1 tt2 -> \(x, y) ->
    extractTypeAssertions tt1 x <>
    extractTypeAssertions tt2 y
  _ -> const []

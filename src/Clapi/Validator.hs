{-# LANGUAGE
    ApplicativeDo
  , LambdaCase
  , GADTs
  , TypeOperators
#-}

module Clapi.Validator
  (validate, validateValues, revalidateValues, extractTypeAssertions
  ) where

import Prelude hiding (fail)
import Control.Monad (join, (>=>), void)
import Control.Monad.Fail (MonadFail(..))
import Data.Constraint (Dict(..))
import Data.Either (partitionEithers)
import qualified Data.Set as Set
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Type.Equality (TestEquality(..), (:~:)(..))
import Data.Word (Word32)
import Text.Regex.PCRE ((=~~))
import Text.Printf (printf, PrintfArg)

import Clapi.Util (ensureUnique)
import Clapi.Types
  ( WireValue(..), SomeWireValue(..), WireType(..), WireTypeOf, Definition
  , DefName, wireTypeOf)
import Clapi.Types.EnumVal (enumVal)
import qualified Clapi.Types.EnumVal as EnumVal
import Clapi.Types.Path (Seg, Path)
import Clapi.TextSerialisation (ttToText)
import qualified Clapi.Types.Path as Path
import Clapi.Types.Tree
  ( Bounds, boundsMin, boundsMax, TreeType(..), SomeTreeType(..)
  , TreeValue(..), SomeTreeValue(..))
import Clapi.Types.UniqList (mkUniqList)
import Clapi.Util (fmtStrictZipError, strictZipWith)

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

validateValue :: MonadFail m => TreeType a -> a -> m ()
validateValue = \case
    TtTime -> void . return
    TtEnum _ -> void . return
    TtWord32 b -> void . inBounds b
    TtWord64 b -> void . inBounds b
    TtInt32 b -> void . inBounds b
    TtInt64 b -> void . inBounds b
    TtFloat b -> void . inBounds b
    TtDouble b -> void . inBounds b
    TtString pat -> void . checkString pat
    TtRef _ -> void . return
    TtList tt -> mapM_ $ validateValue tt
    TtSet tt ->  mapM_ $ validateValue tt
    TtOrdSet tt -> mapM_ $ validateValue tt
    TtMaybe tt -> mapM_ $ validateValue tt
    TtPair tt1 tt2 -> \(a, b) -> validateValue tt1 a >> validateValue tt2 b


typeValid :: MonadFail m => WireType a -> TreeType b -> m (a :~: WireTypeOf b)
typeValid WtTime TtTime = return Refl
typeValid WtWord32 tt1
  | TtWord32 _ <- tt1 = return Refl
  | TtEnum _ <- tt1 = return Refl
typeValid WtWord32 (TtEnum _) = return Refl
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
typeValid wt tt = fail $ printf "Type mismatch: Cannot produce %s from %s"
  (ttToText tt) (show wt)

inflateValue
  :: (MonadFail m)
  => TreeType a -> WireTypeOf a -> m a
inflateValue = \case
    TtTime -> return
    TtEnum sl -> enumVal sl
    TtWord32 b -> return
    TtWord64 b -> return
    TtInt32 b -> return
    TtInt64 b -> return
    TtFloat b -> return
    TtDouble b -> return
    TtString pat -> return
    TtRef _ -> Path.fromText Path.segP
    TtList tt -> mapM $ inflateValue tt
    TtSet tt -> case ordyShowy tt of
      Dict -> mapM (inflateValue tt) >=> ensureUnique "items"
              >=> return . Set.fromList
    TtOrdSet tt -> case ordyShowy tt of
      Dict -> mapM (inflateValue tt) >=> mkUniqList
    TtMaybe tt -> mapM $ inflateValue tt
    TtPair tt1 tt2 -> \(a, b) ->
      (,) <$> inflateValue tt1 a <*> inflateValue tt2 b
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
validate tt (WireValue wt wta) = do
  Refl <- typeValid wt tt
  a <- inflateValue tt wta
  validateValue tt a
  return $ TreeValue tt a

validate_ :: MonadFail m => SomeTreeType -> SomeWireValue -> m SomeTreeValue
validate_ (SomeTreeType tt) (SomeWireValue wv) =
  SomeTreeValue <$> validate tt wv

-- NB: a and b are only different because we can cast between different EnumVal
-- types without necessarily changing the meaning of the values.
revalidate :: MonadFail m => TreeType a -> TreeValue b -> m (TreeValue a)
revalidate tt2 (TreeValue tt1 a) = TreeValue tt2 <$>
  case testEquality tt2 tt1 of
    Nothing -> case (tt1, tt2) of
      (TtEnum _, TtEnum sl2) -> EnumVal.castValue a sl2
      _ -> fail "glamrum"
    Just Refl -> validateValue tt2 a >> return a

revalidate_ :: MonadFail m => SomeTreeType -> SomeTreeValue -> m SomeTreeValue
revalidate_ (SomeTreeType tt) (SomeTreeValue tv) =
  SomeTreeValue <$> revalidate tt tv

zipWithTreeType
  :: (SomeTreeType -> b -> Either String SomeTreeValue) -> [SomeTreeType] -> [b]
  -> Either [Text] [SomeTreeValue]
zipWithTreeType f tts vs = either (Left . pure . Text.pack) collect $
    fmtStrictZipError "types" "values" $ strictZipWith f tts vs
  where
    collect :: [Either String SomeTreeValue] -> Either [Text] [SomeTreeValue]
    collect etvs = let (errs, tvs) = partitionEithers etvs in
      if null errs then Right tvs else Left (Text.pack <$> errs)

validateValues
  :: [SomeTreeType] -> [SomeWireValue] -> Either [Text] [SomeTreeValue]
validateValues = zipWithTreeType validate_

revalidateValues
  :: [SomeTreeType] -> [SomeTreeValue] -> Either [Text] [SomeTreeValue]
revalidateValues = zipWithTreeType revalidate_

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

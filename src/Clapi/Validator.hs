{-# LANGUAGE
    GADTs
  , LambdaCase
  , RankNTypes
  , TypeOperators
#-}

module Clapi.Validator where

import Prelude hiding (fail)

import Control.Monad (void, (>=>))
import Control.Monad.Fail (MonadFail(..))
import Data.Constraint (Dict(..))
import qualified Data.Set as Set
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Type.Equality ((:~:)(..))
import Text.Regex.PCRE ((=~~))
import Text.Printf (printf, PrintfArg)

import Clapi.Util (ensureUnique, foldMapM)
import Clapi.TextSerialisation (ttToText)
import Clapi.Types.Definitions (DefName)
import Clapi.Types.EnumVal (enumVal)
import Clapi.Types.Path (Path)
import qualified Clapi.Types.Path as Path
import Clapi.Types.Tree
import Clapi.Types.UniqList (mkUniqList)
import Clapi.Types.Wire


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
  TtWord32 _ -> return
  TtWord64 _ -> return
  TtInt32 _ -> return
  TtInt64 _ -> return
  TtFloat _ -> return
  TtDouble _ -> return
  TtString _ -> return
  TtRef _ -> Path.fromText Path.segP
  TtList tt -> mapM $ inflateValue tt
  TtSet tt -> case (getTtShow tt, getTtOrd tt) of
    (Dict, Dict) -> mapM (inflateValue tt) >=> ensureUnique "items"
                    >=> return . Set.fromList
  TtOrdSet tt -> case (getTtShow tt, getTtOrd tt) of
    (Dict, Dict) -> mapM (inflateValue tt) >=> mkUniqList
  TtMaybe tt -> mapM $ inflateValue tt
  TtPair tt1 tt2 -> \(a, b) ->
    (,) <$> inflateValue tt1 a <*> inflateValue tt2 b

validate :: MonadFail m => TreeType a -> WireValue b -> m a
validate tt (WireValue wt b) = do
  Refl <- typeValid wt tt
  a <- inflateValue tt b
  validateValue tt a
  return a

validate_ :: MonadFail m => SomeTreeType -> SomeWireValue -> m ()
validate_ (SomeTreeType tt) (SomeWireValue wv) = void $ validate tt wv


extractTypeAssertions
  :: MonadFail m
  => TreeType a -> WireTypeOf a -> m [(DefName, Path)]
extractTypeAssertions = \case
    TtRef tn -> Path.fromText Path.segP >=> return . pure . (Tagged tn,)
    TtList tt -> recurse tt
    TtSet tt -> recurse tt
    TtOrdSet tt -> recurse tt
    TtMaybe tt -> recurse tt
    TtPair tt1 tt2 -> \a -> (<>)
      <$> extractTypeAssertions tt1 (fst a)
      <*> extractTypeAssertions tt2 (snd a)
    _ -> const $ return []
  where
    recurse tt = foldMapM (extractTypeAssertions tt)

extractTypeAssertions_
  :: MonadFail m => SomeTreeType -> SomeWireValue -> m [(DefName, Path)]
extractTypeAssertions_ (SomeTreeType tt) (SomeWireValue (WireValue wt a)) = do
  Refl <- typeValid wt tt  -- FIXME: repetition of this validation from validate
  extractTypeAssertions tt a

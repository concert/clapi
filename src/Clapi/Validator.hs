{-# LANGUAGE
    GADTs
  , LambdaCase
  , RankNTypes
#-}

module Clapi.Validator where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Data.Constraint (Dict(..))
import Data.Word (Word32)
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Regex.PCRE ((=~~))
import Text.Printf (printf, PrintfArg)

import Clapi.Util (ensureUnique, foldMapM)
import Clapi.TextSerialisation (ttToText)
import Clapi.Types (DefName)
import Clapi.Types.Wire
import Clapi.Types.Path (Seg, Path)
import qualified Clapi.Types.Path as Path
import Clapi.Types.Tree (Bounds, boundsMin, boundsMax, TreeType(..))


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


ttWireType :: TreeType -> SomeWireType
ttWireType = \case
  TtTime -> wtTime
  TtEnum _ -> wtWord32
  TtWord32 _ -> wtWord32
  TtWord64 _ -> wtWord64
  TtInt32 _ -> wtInt32
  TtInt64 _ -> wtInt64
  TtFloat _ -> wtFloat
  TtDouble _ -> wtDouble
  TtString _ -> wtString
  TtRef _ -> wtString
  TtList tt -> wtList $ ttWireType tt
  TtSet tt -> wtList $ ttWireType tt
  TtOrdSet tt -> wtList $ ttWireType tt
  TtMaybe tt -> wtMaybe $ ttWireType tt
  TtPair tt1 tt2 -> wtPair (ttWireType tt1) (ttWireType tt2)

validateValue :: MonadFail m => TreeType -> WireType a -> a -> m a
validateValue tt wt a = go wt tt
  where
    go WtTime TtTime = return a
    go WtWord32 _
      | TtEnum ss <- tt = checkEnum ss a
      | TtWord32 b <- tt = inBounds b a
    go WtWord64 (TtWord64 b) = inBounds b a
    go WtInt32 (TtInt32 b) = inBounds b a
    go WtInt64 (TtInt64 b) = inBounds b a
    go WtFloat (TtFloat b) = inBounds b a
    go WtDouble (TtDouble b) = inBounds b a
    go WtString _
      | TtString r <- tt = checkString r a
      | TtRef _ <- tt = Path.fromText Path.segP a >> return a
    go (WtList wt1) _
      | TtList tt1 <- tt = mapM (validateValue tt1 wt1) a
      | TtSet tt1 <- tt = case (getShow wt1, getOrd wt1) of
          (Dict, Dict) -> ensureUnique "items" a >>= mapM (validateValue tt1 wt1)
      | TtOrdSet tt1 <- tt = case (getShow wt1, getOrd wt1) of
          (Dict, Dict) -> ensureUnique "items" a >>= mapM (validateValue tt1 wt1)
    go (WtMaybe wt1) (TtMaybe tt1) = mapM (validateValue tt1 wt1) a
    go (WtPair wt1 wt2) (TtPair tt1 tt2) =
      (,) <$> validateValue tt1 wt1 (fst a) <*> validateValue tt2 wt2 (snd a)
    go _ _ = fail $ printf "%s is not a valid type for %s"
      (show wt) (Text.unpack $ ttToText tt)

validate :: MonadFail m => TreeType -> WireValue a -> m a
validate tt (WireValue wt a) = validateValue tt wt a


extractTypeAssertions'
  :: MonadFail m
  => TreeType -> WireType a -> a -> m [(DefName, Path)]
extractTypeAssertions' tt wt a = go wt tt
  where
    go WtString (TtRef tn) =
        Path.fromText Path.segP a >>= return . pure . (Tagged tn,)
    go (WtList wt1) _
      | TtList tt1 <- tt = foldMapM (extractTypeAssertions' tt1 wt1) a
      | TtSet tt1 <- tt = foldMapM (extractTypeAssertions' tt1 wt1) a
      | TtOrdSet tt1 <- tt = foldMapM (extractTypeAssertions' tt1 wt1) a
    go (WtMaybe wt1) (TtMaybe tt1) = foldMapM (extractTypeAssertions' tt1 wt1) a
    go (WtPair wt1 wt2) (TtPair tt1 tt2) = (<>)
      <$> extractTypeAssertions' tt1 wt1 (fst a)
      <*> extractTypeAssertions' tt2 wt2 (snd a)
    go _ _ = return []

extractTypeAssertions
  :: MonadFail m
  => TreeType -> WireValue a -> m [(DefName, Path)]
extractTypeAssertions tt (WireValue wt a) = extractTypeAssertions' tt wt a

{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

module Clapi.Validator where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad (void, join)
import Data.Word (Word8)
import Data.Maybe (fromJust)
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Text as Text
import Text.Regex.PCRE ((=~~))
import Text.Printf (printf, PrintfArg)

import Clapi.Util (ensureUnique)
import Clapi.Types (UniqList, mkUniqList, WireValue, Time, Wireable, (<|$|>), cast', castWireValue)
import Clapi.Types.Path (Seg, Path, TypeName)
import qualified Clapi.Types.Path as Path
import Clapi.Types.Tree
  ( TreeType(..), TreeConcreteType(..), TreeContainerType(..), TreeContainerTypeName(..), typeEnumOf
  , contTContainedType, Bounds, boundsMin, boundsMax)
import Clapi.Types.TreeTypeProxy (withTtProxy)
import Clapi.TextSerialisation (ttFromText)

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

unpackTreeType :: TreeType -> (TreeConcreteType, [TreeContainerTypeName])
unpackTreeType tt = let (c, ts) = inner tt in (c, reverse ts)
  where
    inner (TtConc t) = (t, [])
    inner (TtCont t) = let (c, ts) = inner $ contTContainedType t in
        (c, (typeEnumOf t) : ts)

-- FIXME: If we had first class tree values we would be able to guarantee that
-- a ref thing was the right type
extractTypeAssertion :: TreeType -> WireValue -> [(TypeName, Path)]
extractTypeAssertion tt wv =
  let
    (concT, contTs) = unpackTreeType tt
    mTn = case concT of
      TcRef tn -> Just tn
      _ -> Nothing
    listMash :: Wireable a => [TreeContainerTypeName] -> (a -> [Path]) -> [Path]
    listMash (_:cts) f = listMash cts (mconcat . map f)
    listMash [] f = maybe [] id $ f <|$|> wv
  in do
    case mTn of
      Nothing -> []
      Just tn -> (tn,) <$> listMash contTs (\v -> [fromJust $ Path.fromText v])

validate' :: (Wireable a, MonadFail m) => TreeType -> a -> m ()
validate' tt a = case tt of
    TtConc tct -> case tct of
      TcTime -> checkWith @Time pure
      TcEnum ns -> checkWith $ checkEnum ns
      TcWord32 b -> checkWith $ inBounds b
      TcWord64 b -> checkWith $ inBounds b
      TcInt32 b -> checkWith $ inBounds b
      TcInt64 b -> checkWith $ inBounds b
      TcFloat b -> checkWith $ inBounds b
      TcDouble b -> checkWith $ inBounds b
      TcString r -> checkWith $ checkString r
      TcRef _ -> checkWith Path.fromText
      TcValidatorDesc -> checkWith ttFromText
    TtCont tct -> case tct of
      TcList tt1 -> withTtProxy tt1 $ checkListWith @[] tt1 pure
      TcSet tt1 -> withTtProxy tt1 $
        checkListWith @[] tt1 $ ensureUnique "items"
      TcOrdSet tt1 -> withTtProxy tt1 $
        checkListWith @[] tt1 $ ensureUnique "items"
      TcMaybe tt1 -> withTtProxy tt1 $ checkListWith @Maybe tt1 pure
      TcPair tt1 tt2 ->
        let
          f :: forall b c m. (Wireable b, Wireable c, MonadFail m) => Proxy b -> Proxy c -> m ()
          f _ _ = cast' @(b, c) a >>= bimapM_ (validate' tt1) (validate' tt2)
        in
          withTtProxy tt1 (\p1 -> withTtProxy tt2 (\p2 -> f p1 p2))
  where
    checkWith :: (Wireable b, MonadFail m) => (b -> m c) -> m ()
    checkWith f = void $ cast' a >>= f

    checkListWith
      :: forall f b c m. (Foldable f, Wireable b, Wireable (f b), MonadFail m)
      => TreeType -> (f b -> m c) -> Proxy b -> m ()
    checkListWith tt' f _ = checkWith @(f b) $
      \l -> mapM_ (validate' tt') l >> void (f l)

validate :: MonadFail m => TreeType -> WireValue -> m ()
validate tt wv = withTtProxy tt f
  where
    f :: forall a m. (Wireable a, MonadFail m) => Proxy a -> m ()
    f _ = castWireValue @a wv >>= validate' tt

bimapM :: Applicative m => (a -> m a') -> (b -> m b') -> (a, b) -> m (a', b')
bimapM fa fb (a, b) = (,) <$> fa a <*> fb b

bimapM_ :: Applicative m => (a -> m ()) -> (b -> m ()) -> (a, b) -> m ()
bimapM_ fa fb = void . bimapM fa fb

checkString :: MonadFail m => Text -> Text -> m Text
checkString r t = maybe
  (fail $ printf "did not match '%s'" r)
  (const $ return t)
  (Text.unpack t =~~ Text.unpack r :: Maybe ())

checkEnum :: MonadFail m => [Seg] -> Word8 -> m Word8
checkEnum ns w = let theMax = fromIntegral $ length ns in
  if w >= theMax
    then fail $ printf "Enum value %v out of range" w
    else return w

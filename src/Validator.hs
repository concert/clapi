{-# LANGUAGE ScopedTypeVariables #-}
module Validator where

import Control.Error.Util (note)
import qualified Data.Text as Text
import Text.Regex.PCRE ((=~~))
import Text.Printf (printf)

import Types (ClapiValue(..), Time(..), Clapiable, fromClapiValue)
import Tree (CanFail)

type Validator = ClapiValue -> CanFail ()
success = Right ()

fromString :: String -> Validator
fromString = undefined

validate :: [Validator] -> [ClapiValue] -> CanFail ()
validate vs cvs
  | length vs > length cvs = Left "Insufficient values"
  | length vs < length cvs = Left "Insufficient validators"
  | otherwise = softValidate vs cvs

-- validate where lengths of lists aren't important
softValidate :: [Validator] -> [ClapiValue] -> CanFail ()
softValidate vs cvs =  foldl (>>) (Right ()) $ zipWith ($) vs cvs

boolValidator :: Validator
boolValidator (CBool _) = success
boolValidator _ = Left "Bad type"  -- FIXME: should say which!

getTimeValidator :: Maybe Time -> Maybe Time -> Validator
getTimeValidator min max (CTime (Time _ _)) = success
getTimeValidator _ _ _ = Left "Bad type"  -- FIXME: should say which!

getNumValidator :: forall a. (Clapiable a, Num a, Ord a, Bounded a, Show a) =>
    Maybe a -> Maybe a -> Validator
getNumValidator min max =
    \cv -> checkType cv >>= boundsValidator
  where
    -- FIXME: should say which type!
    checkType cv = note "Bad type" (fromClapiValue cv :: Maybe a)
    boundsValidator = discard . (bound min max)
    discard = fmap (const ())


bound :: (Num a, Ord a, Bounded a, Show a) => Maybe a -> Maybe a -> a ->
    CanFail a
bound Nothing Nothing = bound' minBound maxBound
bound (Just min) Nothing = bound' min maxBound
bound Nothing (Just max) = bound' minBound max
bound (Just min) (Just max) = bound' min max

bound' :: (Ord a, Num a, Show a) => a -> a -> a -> CanFail a
bound' min max v
  | v >= min && v <= max = Right v
  | otherwise = Left $ printf "%s not between %s and %s" (show v) (show min)
        (show max)

getStringValidator :: Maybe String -> Validator
getStringValidator Nothing (CString t) = success
getStringValidator (Just pattern) (CString t) =
    note errStr ((Text.unpack t) =~~ pattern :: Maybe ())
  where
    errStr = printf "did not match '%s'" pattern
getStringValidator _ _ = Left "Bad type"  -- FIXME: should say which!

getListValidator :: Validator -> Validator
getListValidator itemValidator (CList xs) =
    softValidate (repeat itemValidator) xs

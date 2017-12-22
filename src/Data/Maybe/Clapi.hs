{-# OPTIONS_GHC -Wall -Wno-orphans #-}

module Data.Maybe.Clapi where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail, fail)

toMonoid :: (Monoid a) => Maybe a -> a
toMonoid Nothing = mempty
toMonoid (Just a) = a

fromFoldable :: (Foldable t) => t a -> Maybe (t a)
fromFoldable t
  | null t = Nothing
  | otherwise = Just t

note :: (MonadFail m) => String -> Maybe a -> m a
note s Nothing = fail s
note _ (Just a) = return a

update :: (a -> b -> b) -> Maybe a -> b -> b
update _ Nothing b = b
update f (Just a) b = f a b

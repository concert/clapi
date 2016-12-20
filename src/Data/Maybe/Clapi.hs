module Data.Maybe.Clapi where

toMonoid :: (Monoid a) => Maybe a -> a
toMonoid Nothing = mempty
toMonoid (Just a) = a

fromFoldable :: (Foldable t) => t a -> Maybe (t a)
fromFoldable t
  | null t = Nothing
  | otherwise = Just t

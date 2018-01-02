{-# OPTIONS_GHC -Wall -Wno-orphans #-}

module Clapi.TaggedData
  ( TaggedData, taggedData, eitherTagged, tdAllTags, tdInstanceToTag
  , tdTagToEnum) where

import Data.List (intersect, nub)

data TaggedData e a = TaggedData {
    tdEnumToTag :: e -> Char,
    tdTagToEnum :: Char -> e,
    tdAllTags :: [Char],
    tdTypeToEnum :: a -> e}

tdInstanceToTag :: TaggedData e a -> a -> Char
tdInstanceToTag td = tdEnumToTag td . tdTypeToEnum td

taggedData :: (Enum e, Bounded e) => (e -> Char) -> (a -> e) -> TaggedData e a
taggedData toTag typeToEnum = if nub allTags == allTags
    then TaggedData toTag fromTag allTags typeToEnum
    else error $ "duplicate tag values: " ++ allTags
  where
    tagMap = (\ei -> (toTag ei, ei)) <$> [minBound ..]
    allTags = fst <$> tagMap
    fromTag t = maybe (err t) id $ lookup t tagMap
    err t = error $ "Unrecognised tag: '" ++ [t] ++ "' expecting one of '"
      ++ allTags ++ "'"

eitherTagged
  :: TaggedData e a -> TaggedData f b -> TaggedData (Either e f) (Either a b)
eitherTagged a b = case intersect (tdAllTags a) (tdAllTags b) of
    [] -> TaggedData toTag fromTag allTags typeToEnum
    i -> error $ "Tags overlap: " ++ i
  where
    allTags = (tdAllTags a) ++ (tdAllTags b)
    isATag t = t `elem` tdAllTags a
    toTag = either (tdEnumToTag a) (tdEnumToTag b)
    fromTag t = if isATag t
        then Left $ tdTagToEnum a t
        else Right $ tdTagToEnum b t
    typeToEnum = either (Left <$> tdTypeToEnum a) (Right <$> tdTypeToEnum b)

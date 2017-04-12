module Path where

import Prelude hiding (fail)
import Data.List (isPrefixOf)
import Data.Tuple (swap)
import Control.Monad.Fail (MonadFail, fail)

type Name = String
type Path = [Name]

root :: Path
root = []

up :: Path -> Path
up [] = root
-- FIXME: using Data.Seq would be faster than a built in list for init (removing
-- last element)
up names = init names

splitBasename :: (MonadFail m) => Path -> m (Path, Name)
splitBasename [] = fail "Can't split root path"
splitBasename ns = return . swap $ f ns
  where
    f (n:[]) = (n, [])
    f (n:ns) = (n:) <$> f ns

isParentOf :: Path -> Path -> Bool
isParentOf = isPrefixOf

isChildOf :: Path -> Path -> Bool
isChildOf = flip isParentOf

isParentOfAny :: Path -> [Path] -> Bool
isParentOfAny parent candidates = or $ isParentOf parent <$> candidates

isChildOfAny :: Path -> [Path] -> Bool
isChildOfAny candidateChild parents =
    or $ isChildOf candidateChild <$> parents

childPaths :: Path -> [Name] -> [Path]
childPaths p ns = (p ++) . pure <$> ns

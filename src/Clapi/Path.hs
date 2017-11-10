module Clapi.Path where

import Prelude hiding (fail)
import Data.List (isPrefixOf)
import Data.Tuple (swap)
import Data.Text (Text)
import Control.Monad.Fail (MonadFail, fail)
import Clapi.Util (append)

type Name = Text
newtype Path = Path [Name] deriving (Eq, Show, Ord)

root :: Path
root = Path []

up :: Path -> Path
up (Path []) = root
-- FIXME: using Data.Seq would be faster than a built in list for init (removing
-- last element)
up (Path names) = Path $ init names

splitBasename :: (MonadFail m) => Path -> m (Path, Name)
splitBasename (Path []) = fail "Can't split root path"
splitBasename (Path ns) = return . swap $ Path <$> f ns
  where
    f (n:[]) = (n, [])
    f (n:ns) = (n:) <$> f ns

-- Generate a traversal to the root from the supplied path paired with the
-- child name from which each path was arrived at
pathsAndChildNames :: Path -> [(Path, Maybe Name)]
pathsAndChildNames p = (p, Nothing) : pac p
  where
    pac = maybe [] (\(p, n) -> (p, Just n) : pac p) . splitBasename

(+|) :: Path -> Name -> Path
(+|) (Path p) n = Path $ append p n

isParentOf :: Path -> Path -> Bool
isParentOf (Path a) (Path b) = isPrefixOf a b

isChildOf :: Path -> Path -> Bool
isChildOf = flip isParentOf

isParentOfAny :: Path -> [Path] -> Bool
isParentOfAny parent candidates = or $ isParentOf parent <$> candidates

isChildOfAny :: Path -> [Path] -> Bool
isChildOfAny candidateChild parents =
    or $ isChildOf candidateChild <$> parents

childPaths :: Path -> [Name] -> [Path]
childPaths (Path p) ns = Path . (p ++) . pure <$> ns

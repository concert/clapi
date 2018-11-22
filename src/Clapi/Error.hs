{-# LANGUAGE
    RankNTypes
#-}
module Clapi.Error where

-- | Provides a monad transformer stack that handles soft and hard errors and
--   state

import Control.Lens (Lens', use, assign)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map.Mol (Mol)
import qualified Data.Map.Mol as Mol

type Errs ei = Mol ei Text
type ErrsM s ei = ExceptT (Errs ei) (WriterT (Errs ei) (State s))

runErrsM :: Ord ei => ErrsM s ei a -> s -> (Either (Errs ei) a, s)
runErrsM vsm = first smush . runState (runWriterT $ runExceptT vsm)
  where
    smush :: Ord ei => (Either (Errs ei) a, Errs ei) -> Either (Errs ei) a
    smush (eMolA, mol1) = case eMolA of
      Left mol2 -> Left $ mol1 <> mol2
      Right a -> if null mol1 then return a else Left mol1

report :: Ord ei => ei -> Text -> ErrsM s ei ()
report ei = reports ei . pure

reports :: Ord ei => ei -> [Text] -> ErrsM s ei ()
reports ei = tell . Mol.singletonList ei

abort :: Ord ei => ei -> Text -> ErrsM s ei a
abort ei = aborts ei . pure

aborts :: Ord ei => ei -> [Text] -> ErrsM s ei a
aborts ei = throwError . Mol.singletonList ei

soften :: Ord ei => ErrsM s ei a -> ErrsM s ei (Maybe a)
soften vsm = lift (runExceptT vsm) >>= softEither tell

harden :: Ord ei => ErrsM s ei (Maybe a) -> ErrsM s ei a
harden vsm = vsm >>= maybe (throwError mempty) return

collect :: (Traversable t, Ord ei) => t (ErrsM s ei a) -> ErrsM s ei (t a)
collect = harden . fmap sequence . traverse soften

reportEither :: Ord ei => ei -> Either Text a -> ErrsM s ei (Maybe a)
reportEither ei = softEither $ report ei

reportsEither :: Ord ei => ei -> Either [Text] a -> ErrsM s ei (Maybe a)
reportsEither ei = softEither $ reports ei

abortEither :: Ord ei => ei -> Either Text a -> ErrsM s ei a
abortEither ei = either (abort ei) return

abortsEither :: Ord ei => ei -> Either [Text] a -> ErrsM s ei a
abortsEither ei = either (aborts ei) return

softEither
  :: Ord ei => (e -> ErrsM s ei ()) -> Either e a -> ErrsM s ei (Maybe a)
softEither f = either (\e -> f e >> return Nothing) (return . Just)

modifying'''
  :: (MonadState s m, MonadError e m)
  => Lens' s a -> (a -> Either e a) -> m ()
modifying''' lens f = use lens >>= either throwError (assign lens) . f

modifying''
  :: Ord ei => Lens' s a -> ei -> (a -> Either String a) -> ErrsM s ei ()
modifying'' lens ei f =
  use lens >>= either (abort ei . Text.pack) (assign lens) . f

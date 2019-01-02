{-# LANGUAGE
    RankNTypes
#-}
module Clapi.Types.Error where

-- | Provides a monad transformer stack that handles soft and hard errors and
--   state.

import Control.Lens (Lens', use, assign)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.Bifunctor (first)

type ErrsM s e = ExceptT e (WriterT e (State s))

runErrsM
  :: (Monoid (t e), Foldable t) => ErrsM s (t e) a -> s -> (Either (t e) a, s)
runErrsM m = first smush. runState (runWriterT $ runExceptT m)
  where
    smush (eea, e1) = case eea of
      Left e2 -> Left $ e1 <> e2
      Right a -> if null e1 then Right a else Left e1

soften :: Monoid e => ErrsM s e a -> ErrsM s e (Maybe a)
soften m = lift (runExceptT m) >>= eitherTell

harden :: (Foldable t, Monoid (t e)) => ErrsM s (t e) a -> ErrsM s (t e) a
harden m = do
  (a, es) <- listen m
  if null es then return a else throwError mempty

collect :: (Traversable t, Monoid e) => t (ErrsM s e a) -> ErrsM s e (t a)
collect t = mapM soften t >>= maybe (throwError mempty) return . sequence

eitherTell :: Monoid e => Either e a -> ErrsM s e (Maybe a)
eitherTell = handleEither tell

eitherThrow :: Monoid e => Either e a -> ErrsM s e a
eitherThrow = either throwError return

handleEither
  :: Monoid e2 => (e1 -> ErrsM s e2 ()) -> Either e1 a -> ErrsM s e2 (Maybe a)
handleEither f = either (\e1 -> f e1 >> return Nothing) (return . Just)

castErrs
  :: (Monoid e1, Monoid e2) => (e1 -> e2) -> ErrsM s e1 a -> ErrsM s e2 a
castErrs f m = do
  (ee1a, e1) <- lift $ lift $ runWriterT $ runExceptT m
  case ee1a of
    Left e1' -> throwError $ f $ e1 <> e1'
    Right a -> tell (f e1) >> return a


-- | This is like Control.Lens.modifying except that the update can fail
eitherModifying
  :: (MonadState s m, MonadError e m) => Lens' s a -> (a -> Either e a) -> m ()
eitherModifying lens f = use lens >>= either throwError (assign lens) . f

-- | This is like Control.Lens.modifying but cast into ErrsM
modifying :: Monoid e => Lens' s a -> (a -> ErrsM s e a) -> ErrsM s e ()
modifying lens f = use lens >>= f >>= assign lens

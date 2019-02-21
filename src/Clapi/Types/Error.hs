{-# LANGUAGE
    FlexibleContexts
  , RankNTypes
#-}
module Clapi.Types.Error where

-- | Provides a monad transformer stack that handles soft and hard errors and
--   state.

import Control.Lens (Lens', use, assign)
import Control.Monad.Except as Except
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor.Identity

import Clapi.Util (Mappable(..), Filterable(..), justs)

type ErrsT s e m = ExceptT e (WriterT e (StateT s m))
type ErrsM s e = ErrsT s e Identity

runErrsT
  :: (Monoid (f e), Foldable f, Functor m)
  => ErrsT s (f e) m a -> s -> m (Either (f e) a, s)
runErrsT = runStateT . errsStateT

runErrsM
  :: (Monoid (f e), Foldable f) => ErrsM s (f e) a -> s -> (Either (f e) a, s)
runErrsM = runState . errsStateT

errsStateT
  :: (Monoid (f e), Foldable f, Functor m)
  => ErrsT s (f e) m a -> StateT s m (Either (f e) a)
errsStateT = fmap smush . runWriterT . runExceptT
  where
    smush (eea, e1) = case eea of
      Left e2 -> Left $ e1 <> e2
      Right a -> if null e1 then Right a else Left e1

soften :: (MonadError e m, MonadWriter e m) => m a -> m (Maybe a)
soften m = catchError (Just <$> m) (\e -> tell e >> return Nothing)

harden :: (Foldable f, MonadWriter (f e) m, MonadError (f e) m) => m a -> m a
harden m = do
  (a, es) <- listen m
  if null es then return a else throwError mempty

collect
  :: (Traversable t, MonadError e m, MonadWriter e m)
  => t (m a) -> m (t a)
collect t = mapM soften t >>= maybe (throwError mempty) return . sequence

filterErrs
  :: ( Traversable t
     , MonadError e m
     , MonadWriter e m
     , Filterable t
     , Mappable t (Maybe a) a
     )
  => t (m a) -> m (t a)
filterErrs t = justs <$> mapM soften t

eitherTell :: MonadWriter e m => Either e a -> m (Maybe a)
eitherTell = handleEither tell

eitherThrow :: MonadError e m => Either e a -> m a
eitherThrow = Except.liftEither

handleEither :: Monad m => (e -> m ()) -> Either e a -> m (Maybe a)
handleEither f = either (\e1 -> f e1 >> return Nothing) (return . Just)

castErrs
  :: (Monoid e1, Monoid e2, Monad m)
  => (e1 -> e2) -> ErrsT s e1 m a -> ErrsT s e2 m a
castErrs f m = do
  (ee1a, e1) <- lift $ lift $ runWriterT $ runExceptT m
  case ee1a of
    Left e1' -> throwError $ f $ e1 <> e1'
    Right a -> tell (f e1) >> return a


-- | This is like Control.Lens.modifying except that the update can fail
eitherModifying
  :: (MonadState s m, MonadError e m) => Lens' s a -> (a -> Either e a) -> m ()
eitherModifying lens f = modifying lens $ either throwError return . f

-- FIXME: this doesn't really belong in here any more
-- | This is like Control.Lens.modifying but compatible with ErrsT/ErrsM
modifying :: MonadState s m => Lens' s a -> (a -> m a) -> m ()
modifying lens f = use lens >>= f >>= assign lens

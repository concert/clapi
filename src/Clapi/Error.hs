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

import Clapi.Types.Digests (DataErrorIndex(..), TpId)
import Clapi.Types.Path (Path)

import Debug.Trace


type ErrsM s e = ExceptT e (WriterT e (State s))

runErrsM :: (Monoid e, Eq e) => ErrsM s e a -> s -> (Either e a, s)
runErrsM errsM = first smush . runState (runWriterT $ runExceptT errsM)
  where
    smush :: (Monoid e, Eq e) => (Either e a, e) -> Either e a
    smush (eea, e1) = case eea of
      Left e2 -> Left $ e1 <> e2
      Right a -> if e1 == mempty then Right a else Left e1

runErrsMSoft :: Semigroup e => ErrsM s e a -> s -> (e, Maybe a, s)
runErrsMSoft errsM = smush . runState (runWriterT $ runExceptT errsM)
  where
    smush :: Semigroup e => ((Either e a, e), s) -> (e, Maybe a, s)
    smush ((eea, e1), s) = case eea of
      Left e2 -> (e1 <> e2, Nothing, s)
      Right a -> (e1, Just a, s)


didItMoan :: (Monoid e, Eq e, Show e) => String -> ErrsM s e a -> ErrsM s e a
didItMoan msg m = do
  s <- get
  let ((ea, e), s') = runState (runWriterT $ runExceptT m) s
  put s'
  case ea of
    Left e' -> trace (msg ++ " aborted") $ traceShow (e <> e') $ throwError e'
    Right a -> if (e == mempty)
      then return a
      else trace (msg ++ " reported") $ traceShow e $ tell e >> return a

report :: (Applicative t, Monoid (t e)) => e -> ErrsM s (t e) ()
report = reports . pure

reports :: Monoid e => e -> ErrsM s e ()
reports = tell

abort :: (Applicative t, Monoid (t e)) => e -> ErrsM s (t e) a
abort = aborts . pure

aborts :: Monoid e => e -> ErrsM s e a
aborts = throwError

soften :: Monoid e => ErrsM s e a -> ErrsM s e (Maybe a)
soften vsm = lift (runExceptT vsm) >>= handleEither tell

harden :: (Monoid e, Eq e) => ErrsM s e a -> ErrsM s e a
harden m = do
  (a, es) <- listen m
  if es == mempty then return a else throwError mempty

collect
  :: (Traversable t, Monoid e) => t (ErrsM s e b) -> ErrsM s e (t b)
collect t = mapM soften t >>= maybe (throwError mempty) return . sequence

-- FIXME: I think I also want a version of collect that grabs all the success it
-- can and just notes down the failures

reportEither
  :: (Applicative t, Monoid (t e)) => Either e a -> ErrsM s (t e) (Maybe a)
reportEither = handleEither report

reportsEither :: Monoid e => Either e a -> ErrsM s e (Maybe a)
reportsEither = handleEither reports

abortEither :: (Applicative t, Monoid (t e)) => Either e a -> ErrsM s (t e) a
abortEither = either abort return

abortsEither :: Monoid e => Either e a -> ErrsM s e a
abortsEither = either aborts return

handleEither
  :: Monoid e2 => (e1 -> ErrsM s e2 ()) -> Either e1 a -> ErrsM s e2 (Maybe a)
handleEither f = either (\e -> f e >> return Nothing) (return . Just)

castErrs
  :: (Monoid e1, Monoid e2, Eq e1) => (e1 -> e2) -> ErrsM s e1 a -> ErrsM s e2 a
castErrs f errsM = do
  (ee1a, e1) <- lift $ lift $ runWriterT $ runExceptT errsM
  case ee1a of
    Left e1' -> aborts (f $ e1 <> e1')
    Right a -> unless (e1 == mempty) (reports (f e1)) >> return a

monadFail :: Either String a -> ErrsM s Text a
monadFail = either (aborts . Text.pack) return

type Errs = Mol DataErrorIndex Text

pathError :: Path -> ErrsM s Text a -> ErrsM s Errs a
pathError p = castErrs $ Mol.singleton $ PathError p

pathErrors :: Path -> ErrsM s [Text] a -> ErrsM s Errs a
pathErrors p = castErrs $ Mol.singletonList $ PathError p

timePointError :: Path -> TpId -> ErrsM s Text a -> ErrsM s Errs a
timePointError p tpid = castErrs $ Mol.singleton $ TimePointError p tpid

timePointErrors :: Path -> TpId -> ErrsM s [Text] a -> ErrsM s Errs a
timePointErrors p tpid = castErrs $ Mol.singletonList $ TimePointError p tpid

modifying''
  :: (MonadState s m, MonadError e m)
  => Lens' s a -> (a -> Either e a) -> m ()
modifying'' lens f = use lens >>= either throwError (assign lens) . f

modifying''' :: Monoid e => Lens' s a -> (a -> ErrsM s e a) -> ErrsM s e ()
modifying''' lens f = use lens >>= f >>= assign lens

-- modifying''
--   :: Ord ei => Lens' s a -> ei -> (a -> Either String a) -> ErrsM s ei ()
-- modifying'' lens ei f =
--   use lens >>= either (abort ei . Text.pack) (assign lens) . f

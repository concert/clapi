{-# LANGUAGE
    ConstraintKinds
  , DataKinds
  , FlexibleInstances
  , KindSignatures
  , MultiParamTypeClasses
  , TypeFamilies
  , UndecidableInstances
#-}

module Clapi.Valuespace.ErrWrap where

import Control.Monad.Except (MonadError)
import qualified Control.Monad.Except as E
import Data.Bifunctor (first)
import Data.Kind (Type, Constraint)


class Wraps wrapped wrapper where
  wrap :: wrapped -> wrapper

instance Wraps a a where
  wrap = id

throw :: (Wraps e1 e2, MonadError e2 m) => e1 -> m a
throw = E.throwError . wrap

liftEither :: (Wraps e1 e2, MonadError e2 m) => Either e1 a -> m a
liftEither = E.liftEither . first wrap


type family Errs (errs :: [Type]) (e :: Type) :: Constraint where
  Errs errs e = Errs_ errs e ()

type family Errs_ (errs :: [Type]) (e :: Type) (c :: Constraint) where
  Errs_ '[] e c = c
  Errs_ ('(:) t ts) e c = (Wraps t e, Errs_ ts e c)

type MonadErrors errs e m = (Errs errs e, MonadError e m)


note :: MonadErrors '[e1] e2 m => e1 -> Maybe a -> m a
note err = maybe (throw err) return

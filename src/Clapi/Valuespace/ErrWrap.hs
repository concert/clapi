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

import Control.Monad.Except (MonadError(..))
import Data.Kind (Type, Constraint)


class Wraps wrapped wrapper where
  wrap :: wrapped -> wrapper

instance Wraps a a where
  wrap = id

throw :: (Wraps e1 e2, MonadError e2 m) => e1 -> m a
throw = throwError . wrap


type family Errs (errs :: [Type]) (e :: Type) :: Constraint where
  Errs errs e = Errs_ errs e ()

type family Errs_ (errs :: [Type]) (e :: Type) (c :: Constraint) where
  Errs_ '[] e c = c
  Errs_ ('(:) t ts) e c = (Wraps t e, Errs_ ts e c)

type MonadErrors errs e m = (Errs errs e, MonadError e m)

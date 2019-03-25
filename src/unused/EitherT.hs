module EitherT where

import Control.Exception.Safe   (MonadCatch, tryAny, SomeException)
import Control.Monad.Trans      (MonadTrans (lift))

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT run) = EitherT (fmap (fmap f) run)

instance Applicative m => Applicative (EitherT e m) where
    pure = EitherT . pure . Right
    EitherT x <*> EitherT y = EitherT ((<*>) <$> x <*> y)

instance Monad m => Monad (EitherT e m) where
    EitherT run >>= f = EitherT (run >>= \a -> 
        case a of
            Left l -> return (Left l)
            Right r -> let EitherT z = f r in z)

instance MonadTrans (EitherT e) where
    lift x = EitherT $ x >>= return . Right

liftTry :: MonadCatch m => m a -> EitherT SomeException m a
liftTry x = lift (tryAny x) >>= recover
    where
    recover (Left l) = left l
    recover (Right r) = right r

right :: Monad m => a -> EitherT e m a
right = EitherT . return . Right

left :: Monad m => e -> EitherT e m a
left = EitherT . return . Left

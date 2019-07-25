{-# LANGUAGE InstanceSigs #-}

module MonadTransformerInstances where

newtype IdentityT m a =
  IdentityT
    { runIdentity :: m a
    }

instance (Functor f) => Functor (IdentityT f) where
  fmap :: (a -> b) -> IdentityT f a -> IdentityT f b
  fmap f (IdentityT fa) = IdentityT $ fmap f fa

instance (Applicative f) => Applicative (IdentityT f) where
  pure :: a -> IdentityT f a
  pure a = IdentityT $ pure a
  (<*>) :: IdentityT f (a -> b) -> IdentityT f a -> IdentityT f b
  (IdentityT f) <*> (IdentityT a) = IdentityT $ f <*> a

instance (Monad m) => Monad (IdentityT m) where
  return :: a -> IdentityT m a
  return = pure
  (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (IdentityT ma) >>= f = IdentityT $  ma >>= runIdentity . f
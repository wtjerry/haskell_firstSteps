{-# LANGUAGE InstanceSigs #-}

module MonadTransformerInstances where

newtype IdentityT m a =
  IdentityT
    { runIdentityT :: m a
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
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f

newtype MaybeT m a =
  MaybeT
    { runMaybeT :: m (Maybe a)
    }

instance (Functor f) => Functor (MaybeT f) where
  fmap :: (a -> b) -> MaybeT f a -> MaybeT f b
  fmap g (MaybeT fma) = MaybeT $ (fmap . fmap) g fma

instance (Applicative f) => Applicative (MaybeT f) where
  pure :: a -> MaybeT f a
  pure a = MaybeT $ (pure . pure) a
  (<*>) :: MaybeT f (a -> b) -> MaybeT f a -> MaybeT f b
  (MaybeT fmab) <*> (MaybeT fma) = MaybeT $ (<*>) <$> fmab <*> fma

instance (Monad m) => Monad (MaybeT m) where
  return :: a -> MaybeT m a
  return = pure
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT mma) >>= f =
    MaybeT $ do
      maybeA <- mma
      case maybeA of
        Nothing -> return Nothing
        Just a -> (runMaybeT . f) a
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

newtype EitherT e m a =
  EitherT
    { runEitherT :: m (Either e a)
    }

instance (Functor f) => Functor (EitherT e f) where
  fmap :: (a -> b) -> EitherT e f a -> EitherT e f b
  fmap f (EitherT a) = EitherT $ (fmap . fmap) f a

instance (Applicative f) => Applicative (EitherT e f) where
  pure :: a -> EitherT e f a
  pure a = EitherT $ (pure . pure) a
  (<*>) :: EitherT e f (a -> b) -> EitherT e f a -> EitherT e f b
  (EitherT f) <*> (EitherT a) = EitherT $ (<*>) <$> f <*> a

instance (Monad m) => Monad (EitherT e m) where
  return :: a -> EitherT e m a
  return = pure
  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT mea) >>= f =
    EitherT $ do
      eitherEA <- mea
      case eitherEA of
        (Left e) -> (return . Left) e
        (Right a) -> (runEitherT . f) a

swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor f) => EitherT e f a -> EitherT a f e
swapEitherT (EitherT fea) = EitherT $ swapEither <$> fea

eitherT :: Monad m => (e -> m c) -> (a -> m c) -> EitherT e m a -> m c
eitherT emc amc (EitherT meea) = meea >>= either emc amc

newtype ReaderT r m a =
  ReaderT
    { runReaderT :: r -> m a
    }

instance (Functor f) => Functor (ReaderT r f) where
  fmap :: (a -> b) -> ReaderT r f a -> ReaderT r f b
  fmap f (ReaderT rfa) = ReaderT $ (fmap . fmap) f rfa

instance (Applicative f) => Applicative (ReaderT r f) where
  pure :: a -> ReaderT r f a
  pure a = ReaderT $ (const . pure) a
  -- Alternatively using the Function Applicative:
  --  (ReaderT f) <*> (ReaderT a) = ReaderT $ (<*>) <$> f <*> a
  (<*>) :: ReaderT r f (a -> b) -> ReaderT r f a -> ReaderT r f b
  (ReaderT f) <*> (ReaderT a) = ReaderT $ \r -> f r <*> a r

instance (Monad m) => Monad (ReaderT r m) where
  return :: a -> ReaderT r m a
  return = pure
  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT rma) >>= f = ReaderT $ \r -> rma r >>= \a -> (runReaderT . f) a r
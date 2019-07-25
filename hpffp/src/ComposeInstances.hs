{-# LANGUAGE InstanceSigs #-}

module ComposeInstances where

import Twinplicative

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap am (Compose fga) = (foldMap . foldMap) am fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: Applicative f2 => (a -> f2 b) -> Compose f g a -> f2 (Compose f g b)
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga


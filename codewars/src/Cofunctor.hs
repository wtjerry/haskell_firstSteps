{-# LANGUAGE InstanceSigs #-}

module Cofunctor where

import Data.Functor.Contravariant (Contravariant (..))

newtype Predicate a = Predicate {getPredicate :: a -> Bool}

instance Contravariant Predicate where
  contramap :: (a -> b) -> Predicate b -> Predicate a
  contramap f (Predicate b) = Predicate $ \a -> (b . f) a

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}

module FiveFundamentalMonads where

import Data.Monoid
import Prelude hiding (Identity, Maybe (..), Monad, Reader, State, Writer)

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

data State s a = State {runState :: s -> (a, s)}

data Reader s a = Reader {runReader :: s -> a}

data Writer w a = Writer {runWriter :: (w, a)}

instance Monad (State s) where
  return :: a -> State s a
  return = \a -> State $ \s -> (a, s)
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (State sma) >>= f = State $ \s0 ->
    let (a, s1) = sma s0
     in runState (f a) s1

instance Monad (Reader s) where
  return :: a -> Reader s a
  return = \a -> Reader $ \s -> a
  (>>=) :: Reader s a -> (a -> Reader s b) -> Reader s b
  (Reader r1) >>= f = Reader $ \s ->
    let r2 = (f . r1) s
     in runReader r2 s

instance Monoid w => Monad (Writer w) where
  return :: a -> Writer w a
  return = \a -> Writer $ (mempty, a)
  (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
  (Writer (w1, a)) >>= f =
    Writer $
      let (w2, b) = (runWriter . f) a
       in (mappend w1 w2, b)

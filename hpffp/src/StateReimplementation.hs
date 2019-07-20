{-# LANGUAGE InstanceSigs #-}

module StateReimplementation where

newtype Moi s a =
  Moi
    { runMoi :: s -> (a, s)
    }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi aMoi) =
    Moi $ \s ->
      let (a, newS) = aMoi s
       in (f a, newS)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi fMoi) <*> (Moi aMoi) =
    Moi $ \s1 ->
      let (f, s2) = fMoi s1
          (a, s3) = aMoi s2
       in (f a, s3)

instance Monad (Moi s) where
  return :: a -> Moi s a
  return a = Moi $ \s -> (a, s)
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi aMoi) >>= f =
    Moi $ \s1 ->
      let (a, s2) = aMoi s1
       in runMoi (f a) s2
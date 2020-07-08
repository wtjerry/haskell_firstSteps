{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

-- import Control.Monad (liftM)
-- import Control.Monad.Trans.Class
-- 
-- data Free f a =
--       Pure a
--     | Free (f (Free f a))
-- 
-- instance Functor f => Functor (Free f) where
--     fmap f = go where
--         go (Pure a)  = Pure (f a)
--         go (Free fa) = Free (go <$> fa)
-- 
-- instance Functor f => Applicative (Free f) where
--     pure = Pure
--     Pure a <*> Pure b = Pure $ a b
--     Pure a <*> Free mb = Free $ fmap a <$> mb
--     Free ma <*> b = Free $ (<*> b) <$> ma
-- 
-- instance Functor f => Monad (Free f) where
--     return = pure
--     Pure a >>= f = f a
--     Free m >>= f = Free ((>>= f) <$> m)
-- 
-- 
-- 
-- 
-- foldFree1 :: Monad m => (forall x . f x -> m x) -> Free f a -> m a
-- foldFree1 _ (Pure a)  = return a
-- foldFree1 f (Free as) = f as >>= foldFree1 f
-- 
-- foldFree :: Monad m => (forall x . f x -> m x) -> Free f a -> m a
-- foldFree _ (Pure a)  = return a
-- foldFree f (Free as) = do
--                          r <- f as
--                          foldFree f r
-- 







-- LambdaConf 2015 Give me Freedom or Forgeddaboutit

import Control.Monad
import Control.Monad.Free

data TeletypeF a =
      PutStrLn String a
    | GetLine (String -> a)

instance Functor TeletypeF where
    fmap f (PutStrLn s a) = PutStrLn s (f a)
    fmap f (GetLine fsa) = GetLine $ f . fsa

type Teletype = Free TeletypeF

putStrLnTT :: String -> Teletype ()
putStrLnTT line = liftF (PutStrLn line ())

getLineTT :: Teletype String
getLineTT = liftF (GetLine id)

echoTT :: Teletype ()
echoTT = forever $ do
    line <- getLineTT
    putStrLnTT line

niceConversationTT :: Teletype ()
niceConversationTT = do
    putStrLnTT "hello, what is your name?"
    name <- getLineTT
    putStrLnTT $ "nice to meet you " ++ name
    putStrLnTT "so.. how are you?"
    s <- getLineTT
    putStrLnTT $ "you are " ++ s ++ ", that's " ++ s
    putStrLnTT "bye"



interpreteTT :: TeletypeF a -> IO a
interpreteTT (PutStrLn line a) = putStrLn line >> return a
interpreteTT (GetLine next) = do
    line <- getLine
    return $ next line

echoIO :: IO ()
echoIO = foldFree interpreteTT echoTT

niceConversationIO :: IO ()
niceConversationIO = foldFree interpreteTT niceConversationTT


-- Free: 
-- just means something is free (floating?) -> not compressed, squashed, executed
-- Free Monoid is just a list of values which was not yet folded into one value
-- Free Functor is a Monad because you can bind Functors together (with the Pure Constructure as a "stopping point")

-- interprete:
-- defines how to translate a Functor constructor / "piece" to IO effects
-- `Free SomeFunctor` gives you a Monad for which this translation works even when the Functors are bound together.
-- How exactly does that work?: We know how we can Translate a Functor to an IO effect (eg via interprete). Having a Free case constructor means we have a Functor f and a Free value. The Functor f is easy, just use interprete. The Free instance may either be Pure or another Functor f2 and a Free value2. If it is Pure we are done. If not we need to bind the effects of f and f2 together.


-- how do hfm play into this? will i be able to create a echoTT with type AppL which uses not only Teletype but also other things? how do those other things and Teletype hierarically compose?



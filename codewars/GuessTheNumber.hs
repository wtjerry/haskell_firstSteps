import Control.Monad.Reader

greaterThan :: Int -> Reader Int Bool
greaterThan n = asks (> n)

runGame :: Int -> Int
runGame n = runReader (guess greaterThan) n

guess gt = guess' 50 b gt

b = [25, 13, 7, 4, 2, 1] :: [Int]

guess' :: Monad m => Int -> [Int] -> (Int -> m Bool) -> m Int
guess' n [] gt = do
  g <- gt n
  if g
    then return (n + 1)
    else return n
guess' n (x : xs) gt = do
  g <- gt n
  if g
    then guess' (n + x) xs gt
    else guess' (n - x) xs gt

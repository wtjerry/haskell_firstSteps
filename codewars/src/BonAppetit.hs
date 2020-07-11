module BonAppetit where

import Control.Monad

solve :: Int -> [Int] -> Int -> Maybe Float
solve k items charged
  | moneyDiff == 0 = Nothing
  | otherwise = Just moneyDiff
  where
    totalAnna = (fromIntegral . sum . excludeNth k) items / 2
    moneyDiff = fromIntegral charged - totalAnna

excludeNth :: Int -> [a] -> [a]
excludeNth n l = (init . fst) s ++ snd s
  where
    s = splitAt (n + 1) l

getList :: Read a => IO [a]
getList = do
  line <- getLine
  return $ map read $ words line

main :: IO ()
main = do
  [_, k] <- getList
  items <- getList
  [charged] <- getList
  putStrLn $ maybe "Bon Appetit" show (solve k items charged)

main2 :: IO ()
main2 = do
  [[_, k], items, [charged]] <- replicateM 3 getList
  putStrLn $ maybe "Bon Appetit" show (solve k items charged)

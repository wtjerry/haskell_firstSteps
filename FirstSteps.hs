module FirstSteps where
import Data.List
import Data.Maybe
import System.IO

addMe :: Integer -> Double -> Double
addMe x y = (fromIntegral x) + y

addMe2 :: Double -> Double -> Integer
addMe2 x y = round (x + y)

doSomething x = sum x

doSomething2 :: [Integer] -> Integer
doSomething2 x = sum x

noParam = [1..10]

add2 x y = x + y

add3 :: Num c => c -> c -> c
add3 x y = x + y

sumMaybe :: [Maybe Int] -> Int
sumMaybe x = sum (map unMaybe x)

unMaybe :: Num a => Maybe a -> a
unMaybe (Just x) = x
unMaybe Nothing = 0


addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

whatAge 16 = "you can drive"
whatAge 18 = "you can vote"
whatAge _ = "nothing important"

whatAge2 x
  | x == 16 = "you can drive"
  | x == 18 = "you can vote"
  | otherwise = "nothing important"

times4 :: [Int] -> [Int]
times4 [] = []
--times4 (x:[]) = [4*x]
times4 (x:xs) = [4*x] ++ times4 xs

map2 :: (a -> b) -> [a] -> [b]
map2 f [] = []
--map2 f (x:xs) = [f x] ++ map2 f xs
map2 f (x:xs) = f x : map2 f xs

areStringEq :: [Char] -> [Char] -> Bool
areStringEq [] [] = True
areStringEq (x:xs) (y:ys) = x == y && areStringEq xs ys



getAddFunc :: Int -> (Int -> Int)
getAddFunc x y = x + y

adds3 = getAddFunc 3
fourPlus3 = adds3 4

concat2Strings :: String -> String -> String
concat2Strings x y = x ++ y

intermediateFuncThatHasFirstParamSetButSecondMissing = concat2Strings "hello "
welcome = intermediateFuncThatHasFirstParamSetButSecondMissing "world"



dbl1To10 = map (*2) [1..10]
lambdaTest = map (\x -> x^x + 1) [1..10]



data RPS = Rock | Paper | Scissors
shoot :: RPS -> RPS -> String
shoot Rock Paper = "player 2 wins"
shoot Rock Scissors = "player 1 wins"
shoot Paper Rock = "player 1 wins"
shoot Paper Scissors = "player 2 wins"
shoot Scissors Rock = "player 2 wins"
shoot Scissors Paper = "player 1 wins"
shoot _ _ = "draw"



-- Circle x, y, radius
-- Rectangle x, y, width, height
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
  deriving Show
area :: Shape -> Float
area (Circle _ _ r) = (r^2) * pi
area (Rectangle _ _ w h) = w * h



data Employee = Employee { name :: String,
                           position :: String,
                           idNum :: Int
                           } deriving (Eq, Show)

sam = Employee {name = "sam", position = "manager", idNum = 1}
pam = Employee {name = "pam", position = "sales", idNum = 2}

isSamPam = sam == pam
isSamSam = sam == sam
samData = show sam


data ShirtSize = S | M | L
instance Eq ShirtSize where
  S == S = True
  M == M = True
  L == L = True
  _ == _ = False



writeToFile = do
  theFile <- openFile "test.txt" WriteMode
  hPutStrLn theFile "hello world"
  hClose theFile

fib = 1 : 1 : [a + b | (a, b) <- zip fib (tail fib)]

numberOfDecimalsOf1000thFib = floor (logBase 10 (fromIntegral  (fib !! 1000)))

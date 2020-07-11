{-# LANGUAGE ScopedTypeVariables #-}

module StringsMix where

import Data.Char
import Data.Function
import Data.List

data WhichIsLonger = LeftLonger | RightLonger | Equal deriving (Eq)

instance Ord WhichIsLonger where
  compare a b | a == b = EQ
  compare LeftLonger _ = GT
  compare RightLonger _ = GT
  compare Equal _ = LT

data WhichIsLongerWithSize = MkWhichIsLongerWithSize {whichIsLonger :: WhichIsLonger, l :: Int}

data AOrFiller a = A a | Filler
type WhichIsLongerOrFiller = AOrFiller WhichIsLongerWithSize


mix :: String -> String -> String
mix s1 s2 =
  zipWith takeBigger (prep s1) (prep s2)
    & filterByFiller
    & (map toFinalNotation)
    & (sortBy lengthDescThenCodepointAsc)
    & intercalate "/"

prep :: String -> [(Int, String)]
prep = fillWithFillerValues . (map (\x -> (length x, x))) . group . sort . (filter isLowerAndAlpha)

fillWithFillerValues :: [(Int, String)] -> [(Int, String)]
fillWithFillerValues = f fillerList
  where
    fillerList = map (\c -> (0, c : [])) ['a' .. 'z'] :: [(Int, String)]

    f :: [(Int, String)] -> [(Int, String)] -> [(Int, String)]
    f [] [] = []
    f ((_, fillerH) : fillerT) [] = (0, fillerH) : f fillerT []
    f ((_, fillerH) : fillerT) real@(realH@(_, realHS) : realT) =
      case (compare (head realHS) (head fillerH)) of
        EQ -> realH : (f fillerT realT)
        LT -> error "should never happen, as filler is a strict super set of real"
        GT -> (0, fillerH) : (f fillerT real)
    f [] _ = error "should never happend, as filler is always non empty"

isLowerAndAlpha :: Char -> Bool
isLowerAndAlpha c = (isLower c) && (isAlpha c)

takeBigger :: (Int, String) -> (Int, String) -> (WhichIsLongerOrFiller, String)
takeBigger (iLeft, sLeft) (iRight, sRight) =
  case (compare iLeft iRight) of
    EQ ->
      if iLeft == 0
        then (Filler, "")
        else (A (MkWhichIsLongerWithSize Equal iLeft), sLeft)
    GT -> (A (MkWhichIsLongerWithSize LeftLonger iLeft), sLeft)
    LT -> (A (MkWhichIsLongerWithSize RightLonger iRight), sRight)

filterByFiller :: [(WhichIsLongerOrFiller, String)] -> [(WhichIsLongerWithSize, String)]
filterByFiller = map toNewType . (filter byFillerAndSizeOne)
  where
    toNewType :: (WhichIsLongerOrFiller, String) -> (WhichIsLongerWithSize, String)
    toNewType (Filler, _) = error "should never happen, as filler was filtered out"
    toNewType (A x, s) = (x, s)

    byFillerAndSizeOne :: (WhichIsLongerOrFiller, String) -> Bool
    byFillerAndSizeOne (Filler :: WhichIsLongerOrFiller, _) = False
    byFillerAndSizeOne (A (MkWhichIsLongerWithSize _ 1), _) = False
    byFillerAndSizeOne _ = True

toFinalNotation :: (WhichIsLongerWithSize, String) -> String
toFinalNotation (x, s) = (toChar (whichIsLonger x)) : ":" ++ s
  where
    toChar LeftLonger = '1'
    toChar RightLonger = '2'
    toChar Equal = '='

lengthDescThenCodepointAsc :: String -> String -> Ordering
lengthDescThenCodepointAsc = (flip compare `on` length) <> compare

module ValidParantheses where

validParentheses :: String -> Bool
validParentheses s = helper s 0 0

helper [] o c
  | o == c = True
  | otherwise = False
helper (p : ps) o c
  | o < c = False
  | p == '(' = helper ps (o + 1) c
  | otherwise = helper ps o (c + 1)

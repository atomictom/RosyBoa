module Numbers where

import Data.Char

convert :: Char -> Integer
convert c
  | c == '0' = 0
  | c == '1' = 1
  | c == '2' = 2
  | c == '3' = 3
  | c == '4' = 4
  | c == '5' = 5
  | c == '6' = 6
  | c == '7' = 7
  | c == '8' = 8
  | c == '9' = 9
  | c == 'a' = 10
  | c == 'A' = 10
  | c == 'b' = 11
  | c == 'B' = 11
  | c == 'c' = 12
  | c == 'C' = 12
  | c == 'd' = 13
  | c == 'D' = 13
  | c == 'e' = 14
  | c == 'E' = 14
  | c == 'f' = 15
  | c == 'F' = 15
  | otherwise = error $ "Unrecognized symbol: " ++ [c]

readIntegerBase :: Integer -> String -> Integer
readIntegerBase _ [x] = fromIntegral $ convert x
readIntegerBase b s = go s (0 :: Integer)
  where
    go :: String -> Integer -> Integer
    go [] acc = acc
    go (x:xs) acc = go xs (b * acc + convert x)

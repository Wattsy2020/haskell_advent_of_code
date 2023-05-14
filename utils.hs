module Utils where

import Data.Char (digitToInt)

splitStrAcc :: Char -> Char -> [String] -> [String]
splitStrAcc splitChar c currLines@(line : remaining)
  | splitChar == c = "" : currLines
  | otherwise = (c : line) : remaining

splitStr :: Char -> String -> [String]
splitStr splitChar = foldr (splitStrAcc splitChar) [""]

count :: (a -> Bool) -> [a] -> Int
count predicate = length . filter predicate

strToInt :: String -> Int
strToInt str = foldr1 (\x acc -> x * 10 + acc) (map digitToInt str)

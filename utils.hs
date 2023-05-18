module Utils (splitList, splitStr, count, strToInt) where

import Data.Char (digitToInt)

splitListAcc :: Eq a => [a] -> a -> a -> [[a]] -> [[a]]
splitListAcc initElem splitElem newElem currList@(first : remaining)
  | splitElem == newElem = initElem : currList
  | otherwise = (newElem : first) : remaining

splitList :: Eq a => [a] -> a -> [a] -> [[a]]
splitList initElem splitElem = foldr (splitListAcc initElem splitElem) [initElem]

splitStr :: Char -> String -> [String]
splitStr splitChar = foldr (splitListAcc "" splitChar) [""]

count :: (a -> Bool) -> [a] -> Int
count predicate = length . filter predicate

strToInt :: String -> Int
strToInt str = foldr1 (\x acc -> x * 10 + acc) (map digitToInt str)

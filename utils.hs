{-# LANGUAGE DefaultSignatures #-}

module Utils (splitList, splitStr, count, strToInt, CyclicEnum, succ', pred', filterFoldable, safeInsert, split) where

import Data.Char (digitToInt)
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Map qualified as Map

splitListAcc :: Eq a => [a] -> a -> a -> [[a]] -> [[a]]
splitListAcc initElem splitElem newElem currList@(first : remaining)
  | splitElem == newElem = initElem : currList
  | otherwise = (newElem : first) : remaining

splitList :: Eq a => [a] -> a -> [a] -> [[a]]
splitList initElem splitElem = foldr (splitListAcc initElem splitElem) [initElem]

splitStr :: Char -> String -> [String]
splitStr splitChar = foldr (splitListAcc "" splitChar) [""]

splitAcc :: [String] -> String -> String -> [String]
splitAcc splitStrs@(currString : splitStrs') splitStr remaining
  | remLen < splitLen = (remaining ++ currString) : splitStrs'
  | remTail == splitStr = splitAcc ("" : splitStrs) splitStr remHead -- encounter splitStr, start new section
  | otherwise = splitAcc ((last remaining : currString) : splitStrs') splitStr (init remaining) -- no split, add 1 character to current and continue
  where
    splitLen = length splitStr
    remLen = length remaining
    (remHead, remTail) = splitAt (remLen - splitLen) remaining

-- split toSplit string: splits a string whenever the substring is encountered
split :: String -> String -> [String]
split = splitAcc [""]

count :: (a -> Bool) -> [a] -> Int
count predicate = length . filter predicate

strToInt :: String -> Int
strToInt ('-' : str) = -1 * strToInt str
strToInt str = foldl1 (\acc x -> 10 * acc + x) (map digitToInt str)

filterFoldable :: (Foldable f) => (a -> Bool) -> f a -> [a]
filterFoldable predicate = filter predicate . toList

safeInsert :: Ord k => k -> v -> Map k v -> Map k v
safeInsert = Map.insertWith (\_ _ -> error "Shouldn't be updating existing values")

class CyclicEnum a where
  pred' :: a -> a
  succ' :: a -> a

  default pred' :: (Bounded a, Enum a, Eq a) => a -> a
  pred' x
    | x == (minBound :: a) = maxBound :: a
    | otherwise = pred x

  default succ' :: (Bounded a, Enum a, Eq a) => a -> a
  succ' x
    | x == (maxBound :: a) = minBound :: a
    | otherwise = succ x

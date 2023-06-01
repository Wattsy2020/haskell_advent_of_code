{-# LANGUAGE DefaultSignatures #-}

module Utils (splitList, splitStr, count, strToInt, CyclicEnum, succ', pred', filterFoldable) where

import Data.Char (digitToInt)
import Data.Foldable (toList)

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
strToInt str = foldl1 (\acc x -> 10 * acc + x) (map digitToInt str)

filterFoldable :: (Foldable f) => (a -> Bool) -> f a -> [a]
filterFoldable predicate = filter predicate . toList

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

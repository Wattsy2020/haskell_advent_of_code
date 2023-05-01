import Data.Set qualified as Set
import System.IO
import Text.Parsec (parse)

-- a group of groups of items (either the halves of a rucksack, or the items of each of the three elves)
type ItemGroup = [Set.Set Char]

readItemGroup :: [String] -> ItemGroup
readItemGroup = map Set.fromList

-- part 1
splitRucksack :: String -> ItemGroup
splitRucksack line = readItemGroup [first, second]
  where
    halfLength = length line `div` 2
    (first, second) = splitAt halfLength line

readRucksacks :: String -> [ItemGroup]
readRucksacks contents = map splitRucksack (lines contents)

commonElem :: [Set.Set Char] -> Char
commonElem = Set.findMin . foldl1 Set.intersection -- findMin is a hack to get the single element in the set

-- very inefficient implementation, should learn to use Data.HashMap later
priority :: Char -> Int
priority x = 1 + length (takeWhile (x /=) alphabet)
  where
    alphabet = ['a' .. 'z'] ++ ['A' .. 'Z']

-- calculate the sum of the priorities of the duplicate items
sumPriority :: [ItemGroup] -> Int
sumPriority = sum . map (priority . commonElem)

_readElves :: [String] -> [ItemGroup]
_readElves [] = []
_readElves xs = readItemGroup elfGroup : _readElves elves
  where
    (elfGroup, elves) = splitAt 3 xs

readElves :: String -> [ItemGroup]
readElves = _readElves . lines

main = do
  contents <- readFile "problem_data/day3.txt"
  print (sumPriority (readRucksacks contents), sumPriority (readElves contents))
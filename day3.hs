import Data.Set qualified as Set
import System.IO
import Text.Parsec (parse)

-- store a rucksack as set of the first half and set of the second half
type RuckSack = (Set.Set Char, Set.Set Char)

-- part 1
splitRucksack :: String -> RuckSack
splitRucksack line = (Set.fromList $ take halfLength line, Set.fromList $ drop halfLength line)
  where
    halfLength = length line `div` 2

readRucksacks :: String -> [RuckSack]
readRucksacks contents = map splitRucksack (lines contents)

calcOverlap :: RuckSack -> Char
calcOverlap (a, b) = Set.findMin (Set.intersection a b)

-- very inefficient implementation, should learn to use Data.HashMap later
priority :: Char -> Int
priority x = 1 + length (takeWhile (x /=) alphabet)
  where
    alphabet = ['a' .. 'z'] ++ ['A' .. 'Z']

-- calculate the sum of the priorities of the duplicate items
part1 :: [RuckSack] -> Int
part1 = sum . map (priority . calcOverlap)

main = do
  contents <- readFile "problem_data/day3.txt"
  print $ part1 (readRucksacks contents)
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
calcOverlap (a, b) = Set.findMin $ Set.intersection a b -- findMin is a hack to get the single element in the set

-- very inefficient implementation, should learn to use Data.HashMap later
priority :: Char -> Int
priority x = 1 + length (takeWhile (x /=) alphabet)
  where
    alphabet = ['a' .. 'z'] ++ ['A' .. 'Z']

-- calculate the sum of the priorities of the duplicate items
part1 :: [RuckSack] -> Int
part1 = sum . map (priority . calcOverlap)

type ElfGroup = (Set.Set Char, Set.Set Char, Set.Set Char)

readElfGroup :: String -> String -> String -> ElfGroup
readElfGroup a b c = (Set.fromList a, Set.fromList b, Set.fromList c)

_readElves :: [String] -> [ElfGroup]
_readElves [] = []
_readElves (a : b : c : xs) = readElfGroup a b c : _readElves xs

readElves :: String -> [ElfGroup]
readElves = _readElves . lines

-- calculate the badge item that all three elves share
badge :: ElfGroup -> Char
badge (a, b, c) = Set.findMin $ foldl1 Set.intersection [a, b, c]

part2 :: [ElfGroup] -> Int
part2 = sum . map (priority . badge)

main = do
  contents <- readFile "problem_data/day3.txt"
  print (part1 (readRucksacks contents), part2 (readElves contents))
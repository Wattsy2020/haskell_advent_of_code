import Data.List (reverse, sort)
import System.IO
import Text.Parsec (parse)

accLine :: Char -> [String] -> [String]
accLine '\n' str_list = "" : str_list -- new line so add new string
accLine char (str : str_list) = (char : str) : str_list -- add to previous string (the head as we use fold right)

lines' :: String -> [String]
lines' = foldr accLine [""]

-- accumulate the result of lines into a list representing the calorie content of an elves positions
accParseLine :: String -> [[Int]] -> [[Int]]
accParseLine "" nums = [] : nums -- line break adds a new elf
accParseLine str (num : nums) = ((read str :: Int) : num) : nums

parseLines = foldr accParseLine [[]]

-- parse the file into a multidimensional array, where the ith element represents the total calories an elf holds
parseElves :: String -> [Int]
parseElves contents = map sum ((parseLines . lines') contents)

-- calculate the elf with the most calories
part1 :: String -> Int
part1 contents = maximum (parseElves contents)

-- calculate total calories of top 3 elfs
part2 :: String -> Int
part2 contents = sum (take 3 (reverse (sort (parseElves contents))))

main = do
  contents <- readFile "problem_data/day1.txt"
  print (part1 contents, part2 contents)
import System.IO
import Text.Parsec (parse)

-- note this can be replaced by the `lines` function
_splitLines :: [Char] -> String -> [String] -- split a string into new lines
_splitLines [] prev_line = [prev_line]
_splitLines content prev_line
    | head content == '\n'  = prev_line : _splitLines (tail content) "" -- add prev_line to the list of lines, recurse with empty prev_line
    | otherwise             = _splitLines (tail content) (prev_line ++ [head content]) --recurse while adding to prev_line

splitLines :: [Char] -> [String]
splitLines str = _splitLines str ""

_parseLines :: [String] -> [Int] -> [[Int]]
_parseLines [] nums = [nums]
_parseLines (str:strings) nums
    | str == ""     = nums : _parseLines strings [] -- empty line denotes the start of a new elf, so we have fully parsed the previous elf
    | otherwise     = _parseLines strings ((read str :: Int) : nums)

parseLines :: [String] -> [[Int]]
parseLines strings = _parseLines strings []

-- calculate the elf with the most calories
part1 :: String -> Int
part1 contents = maximum (map sum ((parseLines . splitLines) contents))

main = do
    contents <- readFile "problem_data/day1.txt"
    print (part1 contents)
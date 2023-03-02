import System.IO

-- note this can be replaced by the `lines` function
_splitLines :: [Char] -> String -> [String] -- split a string into new lines
_splitLines [] prev_line = [prev_line]
_splitLines content prev_line
    | head content == '\n'  = prev_line : _splitLines (tail content) "" -- add prev_line to the list of lines, recurse with empty prev_line
    | otherwise             = _splitLines (tail content) (prev_line ++ [head content]) --recurse while adding to prev_line

splitLines str = _splitLines str ""

--TODO: use the empty "" to represent linebreaks (i.e. new elves), convert everything to integer
parseLines :: [String] -> [[Int]]
parseLines str = [[1]]


main = do
    contents <- readFile "problem_data/day1.txt"
    print (splitLines contents)
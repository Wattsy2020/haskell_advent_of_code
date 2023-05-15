import Utils (count, splitStr, strToInt)

data Range = Range Int Int | Empty deriving (Show)

makeRange :: Int -> Int -> Range
makeRange low high
  | low > high = Empty
  | otherwise = Range low high

isNull :: Range -> Bool
isNull Empty = True
isNull _ = False

isSubset :: Range -> Range -> Bool
isSubset (Range low1 high1) (Range low2 high2) = (low1 >= low2) && (high1 <= high2)

anyIsSubset :: Range -> Range -> Bool
anyIsSubset r1 r2 = isSubset r1 r2 || isSubset r2 r1

intersection :: Range -> Range -> Range
intersection _ Empty = Empty
intersection Empty _ = Empty
intersection (Range low1 high1) (Range low2 high2) = makeRange (max low1 low2) (min high1 high2)

parseRange :: String -> Range
parseRange str = Range low high
  where
    [low, high] = map strToInt (splitStr '-' str)

parseLine :: String -> (Range, Range)
parseLine line = (first, second)
  where
    first : second : _ = map parseRange (splitStr ',' line)

parseFile :: String -> [(Range, Range)]
parseFile contents = map parseLine (lines contents)

main = do
  contents <- readFile "problem_data/day4.txt"
  let parsed = parseFile contents
  let part1 = count (uncurry anyIsSubset) parsed
  let part2 = count (not . isNull . uncurry intersection) parsed
  print (part1, part2)

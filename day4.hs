import Utils (count, splitStr, strToInt)

type Assignment = (Int, Int)

parseAssignment :: String -> Assignment
parseAssignment str = (low, high)
  where
    low : high : _ = map strToInt (splitStr '-' str)

parseLine :: String -> (Assignment, Assignment)
parseLine line = (first, second)
  where
    first : second : _ = map parseAssignment (splitStr ',' line)

parseFile :: String -> [(Assignment, Assignment)]
parseFile contents = map parseLine (lines contents)

isSubset :: Assignment -> Assignment -> Bool
isSubset (low1, high1) (low2, high2) = (low1 >= low2) && (high1 <= high2)

anyIsSubset :: (Assignment, Assignment) -> Bool
anyIsSubset (a1, a2) = isSubset a1 a2 || isSubset a2 a1

inRange :: Int -> Int -> Int -> Bool
inRange low high value = low <= value && value <= high

hasOverlap :: (Assignment, Assignment) -> Bool
hasOverlap ((low1, high1), (low2, high2)) = or [inRange low1 high1 low2, inRange low1 high1 low2, inRange low2 high2 low1, inRange low2 high2 high1]

main = do
  contents <- readFile "problem_data/day4.txt"
  let parsed = parseFile contents
  let part1 = count anyIsSubset
  let part2 = count hasOverlap
  print (part1 parsed, part2 parsed)
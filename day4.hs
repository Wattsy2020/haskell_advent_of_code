import Data.Char (digitToInt)
import Data.Set qualified as Set
import Utils (count, splitStr)

type Assignment = Set.Set Int

strToInt :: String -> Int
strToInt str = foldr1 (\x acc -> x * 10 + acc) (map digitToInt str)

parseAssignment :: String -> Assignment
parseAssignment str = Set.fromList [low .. high]
  where
    low : high : _ = map strToInt (splitStr '-' str)

parseLine :: String -> (Assignment, Assignment)
parseLine line = (first, second)
  where
    first : second : _ = map parseAssignment (splitStr ',' line)

parseFile :: String -> [(Assignment, Assignment)]
parseFile contents = map parseLine (lines contents)

anyIsSubset :: (Assignment, Assignment) -> Bool
anyIsSubset (first, second) = Set.isSubsetOf first second || Set.isSubsetOf second first

hasOverlap :: (Assignment, Assignment) -> Bool
hasOverlap = not . Set.null . uncurry Set.intersection

main = do
  contents <- readFile "problem_data/day4.txt"
  let parsed = parseFile contents
  let part1 = count anyIsSubset
  let part2 = count hasOverlap
  print (part1 parsed, part2 parsed)
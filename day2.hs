-- TODO: this whole code really needs enums
import System.IO

-- 1 = Rock
-- 2 = Paper
-- 3 = Scissors
parseShape :: Char -> Int
parseShape str
  | str == 'A' = 1
  | str == 'X' = 1
  | str == 'B' = 2
  | str == 'Y' = 2
  | str == 'C' = 3
  | str == 'Z' = 3

-- parse a line into a list of two Ints, 1st for opponent's shape and 2nd for your shape
parseStrategy :: String -> (Int, Int)
parseStrategy line = (parseShape char1, parseShape char2)
  where
    split = words line
    char1 = head (split !! 0)
    char2 = head (split !! 1)

parseStrategies :: String -> [(Int, Int)]
parseStrategies contents = map parseStrategy (lines contents)

-- return the shape that wins against the input
-- todo: this is basically just defining a group of (1, 2, 3) with + - operations, probably haskell has a way to do this
winingShape :: Int -> Int
winingShape a
  | a == 1 = 2
  | a == 2 = 3
  | a == 3 = 1

losingShape :: Int -> Int
losingShape a
  | a == 1 = 3
  | a == 2 = 1
  | a == 3 = 2

-- 0 for loss, 3 for draw, 6 for win
-- a win is when b wins over a (convention of the question)
scoreMatch :: (Int, Int) -> Int
scoreMatch (a, b)
  | b == winingShape a = 6
  | b == a = 3
  | b == losingShape a = 0

scoreGame :: (Int, Int) -> Int
scoreGame (a, b) = b + scoreMatch (a, b)

-- return the shape need to accomplish a match outcome
shapeForOutcome :: (Int, Int) -> Int
shapeForOutcome (opponentShape, outcome)
  | outcome == 1 = losingShape opponentShape -- loss
  | outcome == 2 = opponentShape -- draw
  | outcome == 3 = winingShape opponentShape -- win

scoreGame2 (opponentShape, outcome) = yourShape + scoreMatch (opponentShape, yourShape)
  where
    yourShape = shapeForOutcome (opponentShape, outcome)

part1 :: String -> Int
part1 contents = sum (map scoreGame (parseStrategies contents))

part2 :: String -> Int
part2 contents = sum (map scoreGame2 (parseStrategies contents))

main = do
  contents <- readFile "problem_data/day2.txt"
  print (part1 contents, part2 contents)
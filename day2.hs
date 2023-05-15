data Shape = Rock | Paper | Scissors deriving (Eq)

data Outcome = Lose | Draw | Win

-- 1 = Rock
-- 2 = Paper
-- 3 = Scissors
parseShape :: Char -> Shape
parseShape 'A' = Rock
parseShape 'X' = Rock
parseShape 'B' = Paper
parseShape 'Y' = Paper
parseShape 'C' = Scissors
parseShape 'Z' = Scissors

parseOutcome :: Char -> Outcome
parseOutcome 'X' = Lose
parseOutcome 'Y' = Draw
parseOutcome 'Z' = Win

-- parse a line into a list of two Ints, 1st for opponent's shape and 2nd for your shape
parseStrategy :: String -> (Shape, Shape)
parseStrategy line = (parseShape char1, parseShape char2)
  where
    [[char1], [char2]] = words line

parseStrategies :: String -> [(Shape, Shape)]
parseStrategies contents = map parseStrategy (lines contents)

parseStrategy2 :: String -> (Shape, Outcome)
parseStrategy2 line = (parseShape char1, parseOutcome char2)
  where
    [[char1], [char2]] = words line

parseStrategies2 :: String -> [(Shape, Outcome)]
parseStrategies2 contents = map parseStrategy2 (lines contents)

-- return the shape that wins against the input
winingShape :: Shape -> Shape
winingShape Rock = Paper
winingShape Paper = Scissors
winingShape Scissors = Rock

losingShape :: Shape -> Shape
losingShape Rock = Scissors
losingShape Paper = Rock
losingShape Scissors = Paper

-- 0 for loss, 3 for draw, 6 for win
-- a win is when b wins over a (convention of the question)
scoreMatch :: (Shape, Shape) -> Int
scoreMatch (a, b)
  | b == winingShape a = 6
  | b == a = 3
  | otherwise = 0

scoreShape :: Shape -> Int
scoreShape Rock = 1
scoreShape Paper = 2
scoreShape Scissors = 3

scoreGame :: (Shape, Shape) -> Int
scoreGame (a, b) = scoreShape b + scoreMatch (a, b)

-- return the shape need to accomplish a match outcome
shapeForOutcome :: Outcome -> Shape -> Shape
shapeForOutcome Lose opponentShape = losingShape opponentShape
shapeForOutcome Draw opponentShape = opponentShape
shapeForOutcome Win opponentShape = winingShape opponentShape

scoreGame2 :: (Shape, Outcome) -> Int
scoreGame2 (opponentShape, outcome) = scoreShape yourShape + scoreMatch (opponentShape, yourShape)
  where
    yourShape = shapeForOutcome outcome opponentShape

part1 :: String -> Int
part1 contents = sum (map scoreGame (parseStrategies contents))

part2 :: String -> Int
part2 contents = sum (map scoreGame2 (parseStrategies2 contents))

main = do
  contents <- readFile "problem_data/day2.txt"
  print (part1 contents, part2 contents)
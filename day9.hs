import Data.Foldable (toList)
import Data.List (nub)
import Linear.V2 (V2 (..))
import Utils (strToInt)
import Prelude hiding (Left, Right)

data Direction = Left | Right | Up | Down deriving (Show)

data Step = Step {direction :: Direction, distance :: Int} deriving (Show)

parseDirection :: Char -> Direction
parseDirection 'L' = Left
parseDirection 'R' = Right
parseDirection 'U' = Up
parseDirection 'D' = Down

parseStep :: String -> Step
parseStep line =
  let [[direction], distance] = words line
   in Step (parseDirection direction) (strToInt distance)

parseSteps :: String -> [Step]
parseSteps = map parseStep . lines

moveHead :: Direction -> V2 Int -> V2 Int
moveHead Left = (+ V2 (-1) 0)
moveHead Right = (+ V2 1 0)
moveHead Up = (+ V2 0 1)
moveHead Down = (+ V2 0 (-1))

-- distance where being 1 square away diagonally is 1 distance
chebyshevNorm :: V2 Int -> Int
chebyshevNorm = maximum . toList . abs

moveTail :: V2 Int -> V2 Int -> V2 Int
moveTail headPos tailPos
  -- if head is within 1 square, then no change to the tail
  | chebyshevNorm diff <= 1 = tailPos
  -- move a maximum of 1 square in the direction we need
  -- note this handles the diagonal cass as well, in which case we move 1 on both x and y axis in the direction necessary
  | otherwise = tailPos + fmap (\x -> min (abs x) 1 * signum x) diff
  where
    diff = headPos - tailPos

-- move the Head one step in Direction, then update the tail positions
moveRope :: (V2 Int, V2 Int) -> Direction -> (V2 Int, V2 Int)
moveRope (head, tail) direction = (newHead, newTail)
  where
    newHead = moveHead direction head
    newTail = moveTail newHead tail

unrollStep :: Step -> [Direction]
unrollStep (Step direction distance) = replicate distance direction

calcPositions :: [Step] -> (V2 Int, V2 Int) -> [(V2 Int, V2 Int)]
calcPositions steps positions = scanl moveRope positions (concatMap unrollStep steps)

main = do
  contents <- readFile "problem_data/day9.txt"
  let steps = parseSteps contents
  let initPos = (V2 0 0, V2 0 0)
  let positions = calcPositions steps initPos
  let part1 = (length . nub . map snd) positions
  -- print steps
  -- print positions
  print part1
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Sequence (Seq (..), (|>))
import Data.Sequence qualified as Seq
import Utils (strToInt)

data Op = Noop | Addx Int deriving (Show)

parseOp :: [String] -> Op
parseOp ["noop"] = Noop
parseOp ["addx", number] = Addx $ strToInt number

parseOps :: String -> [Op]
parseOps = map (parseOp . words) . lines

-- evaluate the given ops
-- prevCycles is a sequence where element i is the value at the start of the (i + 1)th cycle
-- e.g. initially we have 1 |> noop makes it 1 |> 1, addx 3 makes it 1 |> 1 |> 1 |> 4
evalOp :: Seq Int -> Op -> Seq Int
evalOp seq@(_ :|> last) Noop = seq |> last
evalOp seq@(_ :|> last) (Addx num) = seq |> last |> (last + num)

evalOps :: [Op] -> Seq Int
evalOps = foldl evalOp (Seq.singleton 1)

part1 :: [Op] -> Int
part1 ops = sum signalStrengths
  where
    cycleList = evalOps ops
    signalStrengths = map (\x -> x * Seq.index cycleList (x - 1)) [20, 60, 100, 140, 180, 220]

drawPixel :: Int -> Int -> Bool
drawPixel cycleNum x = abs (cycleNum - x) <= 1

-- given the value of X at each cycle, set the pixel values to on or off
drawPixels :: Seq Int -> [[Bool]]
drawPixels Seq.Empty = [[]]
drawPixels xs =
  let (row, remaining) = Seq.splitAt 40 xs
   in zipWith drawPixel [0 ..] (toList row) : drawPixels remaining

showPixel :: Bool -> Char
showPixel True = '#'
showPixel False = '.'

showPixels :: [[Bool]] -> String
showPixels = intercalate "\n" . map (map showPixel)

part2 :: [Op] -> String
part2 ops =
  -- drop the "start of the last cycle" as this isn't rendered
  let (initXs :|> _) = evalOps ops
   in (showPixels . drawPixels) initXs

main = do
  contents <- readFile "problem_data/day10.txt"
  let ops = parseOps contents
  print $ part1 ops
  putStrLn $ part2 ops
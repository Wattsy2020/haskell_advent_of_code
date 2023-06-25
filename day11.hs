import Data.List (stripPrefix)
import Data.Maybe (fromJust)
import Utils (split, strToInt)

data Monkey = Monkey {items :: [Int], operation :: Int -> Int, test :: Int -> Bool, trueTarget :: Int, falseTarget :: Int}

instance Show Monkey where
  show :: Monkey -> String
  show (Monkey items _ _ trueTarget falseTarget) = show items ++ " trueTarget=" ++ show trueTarget ++ " falseTarget=" ++ show falseTarget

parseItems :: String -> [Int]
parseItems = map strToInt . split ", " . fromJust . stripPrefix "  Starting items: "

parseOperand :: String -> Int -> Int
parseOperand "old" oldVal = oldVal
parseOperand num _ = strToInt num

-- evaluation a binary operation on either a variable "old" or an integer
evalOp :: (String, String, String) -> Int -> Int
evalOp (a1, op, a2) old =
  case op of
    "*" -> a1' * a2'
    "+" -> a1' + a2'
  where
    a1' = parseOperand a1 old
    a2' = parseOperand a2 old

parseOperation :: String -> (Int -> Int)
parseOperation opStr = evalOp (a1, op, a2)
  where
    opStr' = fromJust $ stripPrefix "  Operation: new = " opStr
    [a1, op, a2] = words opStr'

parseTest :: String -> (Int -> Bool)
parseTest testStr = \x -> mod x divTestInt == 0
  where
    divTestInt = (strToInt . fromJust . stripPrefix "  Test: divisible by ") testStr

parseTarget :: String -> Int
parseTarget = strToInt . last . words

parseMonkey :: String -> Monkey
parseMonkey monkeyStr =
  let [_, items, operation, test, trueTarget, falseTarget] = lines monkeyStr
   in Monkey (parseItems items) (parseOperation operation) (parseTest test) (parseTarget trueTarget) (parseTarget falseTarget)

parseContents :: String -> [Monkey]
parseContents = map parseMonkey . split "\n\n"

main = do
  contents <- readFile "problem_data/day11.txt"
  print $ parseContents contents

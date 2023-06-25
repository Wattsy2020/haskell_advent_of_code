{-# LANGUAGE RecordWildCards #-}

import Data.List (stripPrefix)
import Data.Maybe (fromJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
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

parseContents :: String -> Seq Monkey
parseContents = Seq.fromList . map parseMonkey . split "\n\n"

addItem :: Int -> Monkey -> Monkey
addItem item Monkey {..} = Monkey (items ++ [item]) operation test trueTarget falseTarget

clearItems :: Monkey -> Monkey
clearItems Monkey {..} = Monkey [] operation test trueTarget falseTarget

monkeyInspect :: Monkey -> Seq Monkey -> Int -> Seq Monkey
monkeyInspect monkey monkeys item = Seq.adjust (addItem worryLevel) monkeyTarget monkeys
  where
    worryLevel = div (operation monkey item) 3
    monkeyTarget = if test monkey worryLevel then trueTarget monkey else falseTarget monkey

monkeyTurn :: Seq Monkey -> Int -> Seq Monkey
monkeyTurn monkeys idx = result
  where
    monkey = Seq.index monkeys idx
    addedItems = foldl (monkeyInspect monkey) monkeys (items monkey)
    result = Seq.adjust clearItems idx addedItems

monkeyRound :: Seq Monkey -> Seq Monkey
monkeyRound monkeys = foldl monkeyTurn monkeys [0 .. length monkeys - 1]

main = do
  contents <- readFile "problem_data/day11.txt"
  let monkeys = parseContents contents
  print $ iterate monkeyRound monkeys !! 20

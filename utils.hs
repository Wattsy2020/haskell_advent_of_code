module Utils where

splitStrAcc :: Char -> Char -> [String] -> [String]
splitStrAcc splitChar c currLines@(line : remaining)
  | splitChar == c = "" : currLines
  | otherwise = (c : line) : remaining

splitStr :: Char -> String -> [String]
splitStr splitChar = foldr (splitStrAcc splitChar) [""]

count :: (a -> Bool) -> [a] -> Int
count predicate = length . filter predicate

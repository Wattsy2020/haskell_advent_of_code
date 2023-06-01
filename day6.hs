import Data.Foldable (toList)
import Data.List qualified as List
import Data.Maybe (fromJust)
import Data.Sequence (Seq ((:<|), (:|>)), (<|), (|>))
import Data.Sequence qualified as Seq

accSeq :: Seq Char -> Char -> Seq Char
accSeq (head :<| tail) newChar = tail |> newChar

numUnique :: Eq a => Seq a -> Int
numUnique = length . List.nub . toList

firstMarker :: String -> Int -> Int
firstMarker contents markerLength = markerLength + fromJust (List.findIndex (\x -> numUnique x == markerLength) fourSeqs)
  where
    (init, remaining) = splitAt markerLength contents
    fourSeqs = scanl accSeq (Seq.fromList init) remaining

main = do
  contents <- readFile "problem_data/day6.txt"
  let part1 = firstMarker contents 4
  let part2 = firstMarker contents 14
  print (part1, take 14 $ drop (part1 - 10) contents)
  print (part2, take 24 $ drop (part2 - 24) contents)
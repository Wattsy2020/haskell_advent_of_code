{-# LANGUAGE RecordWildCards #-}

import Data.Char (isNumber)
import Data.List (elemIndices, intercalate)
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Utils (splitList, splitStr, strToInt)

data Procedure = Procedure {quantity :: Int, source :: Int, target :: Int} deriving (Show)

type CrateCol = Seq.Seq Char

type CrateMap = Map Int CrateCol

-- what we want is a map of colnumber -> stack of elems in that column
-- so build a list(tuple(colnumber, elemchar)) and then add that into a Map (there is a method to customise behaviour)
-- when adding a key that already exists

-- note mapping string index to crate index involves dividing by 4,
-- as there are three [_] chars and one space between each column
parseCrateItem :: Int -> String -> (Int, CrateCol)
parseCrateItem idx itemStr = (idx `div` 4 + 1, Seq.singleton $ head itemStr)

parseCrateLine :: String -> [(Int, CrateCol)]
parseCrateLine line = zipWith parseCrateItem indices elems
  where
    indices = elemIndices '[' line
    elems = tail $ splitStr '[' line

parseCrates :: [String] -> CrateMap
parseCrates crateStr = colMaps
  where
    crateMaps = map (Map.fromList . parseCrateLine) (init crateStr) -- parse each line
    -- join lines from the bottom up, so that the top elements are at start of the list
    colMaps = foldr1 (Map.unionWith (Seq.><)) crateMaps

parseProcedure :: String -> Procedure
parseProcedure line = Procedure quantity source target
  where
    [quantity, source, target] = map strToInt (filter (isNumber . head) (words line))

parseFile :: String -> (CrateMap, [Procedure])
parseFile contents = (crates, procedures)
  where
    [crateStr, procedureStr] = splitList [] "" (lines contents)
    crates = parseCrates crateStr
    procedures = map parseProcedure procedureStr

-- crates are moved such that the first one on left is placed into the right first, and so on
insertCrates1 :: CrateCol -> CrateCol -> CrateCol
insertCrates1 source target = Seq.reverse source Seq.>< target

insertCrates2 :: CrateCol -> CrateCol -> CrateCol
insertCrates2 = (Seq.><)

applyProcedure :: (CrateCol -> CrateCol -> CrateCol) -> CrateMap -> Procedure -> CrateMap
applyProcedure insertCrates crateMap Procedure {..} = insertedNew
  where
    (toBeMoved, leftInPlace) = Seq.splitAt quantity (crateMap ! source)
    removedOld = Map.insert source leftInPlace crateMap
    insertedNew = Map.insertWith insertCrates target toBeMoved removedOld

-- now just do a foldl with initial value as the Map, with a function that takes a procedure and modifies the map
calculateArrangement :: CrateMap -> [Procedure] -> (CrateCol -> CrateCol -> CrateCol) -> [Char]
calculateArrangement crateMap procedures insertCrates = topElems
  where
    reduced = foldl (applyProcedure insertCrates) crateMap procedures
    topElems = map (`Seq.index` 0) $ Map.elems reduced

procedureToStr :: Procedure -> String
procedureToStr Procedure {..} = "move " ++ show quantity ++ " from " ++ show source ++ " to " ++ show target

main = do
  contents <- readFile "problem_data/day5.txt"
  let (crateMap, procedures) = parseFile contents
  print $ map (calculateArrangement crateMap procedures) [insertCrates1, insertCrates2]

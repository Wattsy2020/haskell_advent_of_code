import Data.Char (digitToInt)
import Utils (count)

type Grid a = [[a]]

parseGrid :: String -> Grid Int
parseGrid contents = map (map digitToInt) (lines contents)

_visibleList :: Int -> [Int] -> [Bool]
_visibleList _ [] = []
_visibleList prevMax (x : xs)
  | x > prevMax = True : _visibleList x xs
  | otherwise = False : _visibleList prevMax xs

visibleList :: [Int] -> [Bool]
visibleList = _visibleList (-1)

emptyGrid :: Int -> Grid a
emptyGrid 0 = [[]]
emptyGrid nRows = [] : emptyGrid (nRows - 1)

-- add row i as column i to a transposed grid
addRow :: [a] -> Grid a -> Grid a
addRow = zipWith (:)

-- convert each row into a column
transpose :: Grid a -> Grid a
transpose grid = foldr addRow (emptyGrid $ length grid) grid

visibleLeft :: Grid Int -> Grid Bool
visibleLeft = map visibleList

visibleTop :: Grid Int -> Grid Bool
visibleTop = transpose . visibleLeft . transpose

visibleRight :: Grid Int -> Grid Bool
visibleRight = map (reverse . visibleList . reverse)

visibleBottom :: Grid Int -> Grid Bool
visibleBottom = reverse . visibleTop . reverse

unionGrid :: Grid Bool -> Grid Bool -> Grid Bool
unionGrid = zipWith (zipWith (||))

getVisible :: Grid Int -> Grid Bool
getVisible grid = foldl1 unionGrid [left, top, right, bottom]
  where
    left = visibleLeft grid
    top = visibleTop grid
    right = visibleRight grid
    bottom = visibleBottom grid

part1 :: Grid Int -> Int
part1 = sum . map (count id) . getVisible

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x : xs) = x : if p x then takeWhileInclusive p xs else []

_viewDistance :: [Int] -> [Int] -> [Int]
_viewDistance _ [] = []
_viewDistance prevTrees (x : xs) = (length . takeWhileInclusive (< x)) prevTrees : _viewDistance (x : prevTrees) xs

viewDistance :: [Int] -> [Int]
viewDistance = _viewDistance []

viewDistLeft :: Grid Int -> Grid Int
viewDistLeft = map viewDistance

viewDistTop :: Grid Int -> Grid Int
viewDistTop = transpose . viewDistLeft . transpose

viewDistRight :: Grid Int -> Grid Int
viewDistRight = map (reverse . viewDistance . reverse)

viewDistBottom :: Grid Int -> Grid Int
viewDistBottom = reverse . viewDistTop . reverse

calcScenic :: Grid Int -> Grid Int
calcScenic grid = foldl1 (zipWith (zipWith (*))) [left, top, right, bottom]
  where
    left = viewDistLeft grid
    top = viewDistTop grid
    right = viewDistRight grid
    bottom = viewDistBottom grid

part2 :: Grid Int -> Int
part2 = maximum . map maximum . calcScenic

main = do
  contents <- readFile "problem_data/day8.txt"
  let grid = parseGrid contents

  {-
  print grid
  print $ transpose grid

  let left = visibleLeft grid
  let top = visibleTop grid
  let right = visibleRight grid
  let bottom = visibleBottom grid

  print left
  print top
  print right
  print bottom
  -}

  print $ part1 grid
  print $ part2 grid
import Data.Function (on)
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Sequence (Seq ((:<|), (:|>)), (<|), (|>))
import Data.Sequence qualified as Seq
import Debug.Trace (trace)
import Utils (safeInsert, strToInt)

-- Need a Tree to store the directory structure
data DirTree a = File {name :: String, size :: a} | Dir {name :: String, size :: a, children :: Map String (DirTree a)} deriving (Show)

type FSTree = DirTree Int

instance Foldable DirTree where
  foldr :: (a -> b -> b) -> b -> DirTree a -> b
  foldr f acc (File _ size) = f size acc
  foldr f acc (Dir _ size children) = f size $ foldr (flip (foldr f)) acc children

{-
instance Functor DirTree where
  fmap :: (a -> b) -> DirTree a -> DirTree b
  fmap f (File name size) = File name (f size)
  fmap f (Dir name children) = Dir name (fmap (fmap f) children)
-}

makeDir :: String -> FSTree
makeDir name = Dir name 0 Map.empty

-- And a sequence of commands to represent the position in the tree (e.g. cd dir1, cd dir2, cd dir3), then cd .. just pops off the most recent command
data Command = Root | Parent | CD String | LS deriving (Show)

getFile :: Seq Command -> FSTree -> FSTree
getFile Seq.Empty dirTree = dirTree
getFile ((CD dirname) :<| remaining) (Dir _ _ children) = getFile remaining (children ! dirname)

insertFile :: FSTree -> Seq Command -> FSTree -> FSTree
insertFile toInsert Seq.Empty (Dir dirName dirSize children) = Dir dirName (dirSize + size toInsert) $ safeInsert (name toInsert) toInsert children
insertFile toInsert ((CD dirname) :<| remaining) (Dir dirName dirSize children) = Dir dirName (dirSize + size toInsert) (Map.insert dirname updatedChild children)
  where
    updatedChild = insertFile toInsert remaining (children ! dirname)

parseCommand :: [String] -> Command
parseCommand ["ls"] = LS
parseCommand ("cd" : remaining) = case remaining of
  ["/"] -> Root
  [".."] -> Parent
  [name] -> CD name

addCommand :: Seq Command -> Command -> Seq Command
addCommand _ Root = Seq.empty
addCommand (init :|> _) Parent = init
addCommand commands com@(CD _) = commands |> com
addCommand commands LS = commands -- ignore ls, our code will parse the next lines anyway

parseFile :: String -> String -> Seq Command -> FSTree -> FSTree
parseFile info name = insertFile toInsert
  where
    toInsert = case info of
      "dir" -> makeDir name
      fileSize -> File name (strToInt fileSize)

parseLine :: (Seq Command, FSTree) -> String -> (Seq Command, FSTree)
parseLine (commands, dirTree) line = case words line of
  ("$" : remaining) -> (addCommand commands (parseCommand remaining), dirTree)
  [info, name] -> (commands, parseFile info name commands dirTree)

parseLines :: String -> (Seq Command, FSTree)
parseLines = foldl parseLine (Seq.empty, makeDir "/") . lines

_filterDir :: (FSTree -> Bool) -> FSTree -> [FSTree] -> [FSTree]
_filterDir _ (File _ _) filtered = filtered
_filterDir f dir filtered
  | f dir = dir : filteredChildren
  | otherwise = filteredChildren
  where
    filteredChildren = foldr (_filterDir f) filtered (children dir)

filterDir :: (FSTree -> Bool) -> FSTree -> [FSTree]
filterDir f dir = _filterDir f dir []

part1 :: FSTree -> Int
part1 = sum . map size . filterDir (\x -> size x <= 100_000)

part2 :: FSTree -> Int
part2 = minimum . map size . filterDir (\x -> size x >= 8381165)

main = do
  contents <- readFile "problem_data/day7.txt"
  let (commands, dirTree) = parseLines contents
  print commands
  print dirTree
  print $ size dirTree -- bug in the insertion code, directory size is smaller than what the question says
  print $ part1 dirTree
  print $ part2 dirTree
  print $ map (\x -> (name x, size x)) $ filterDir (\x -> size x >= 8381165) dirTree
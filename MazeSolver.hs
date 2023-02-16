module MazeSolver where

import Control.Applicative
import Data.Map (Map)
import Data.Map qualified as Map
import System.Environment (getArgs)

type Path = [Pos]

type Pos = (Row, Col)

type Row = Int

type Col = Int

type MazeMap = Map Pos Char

----------------------------------------------------------------------------------
-- Loading data --
----------------------------------------------------------------------------------

-- load data from file into a Map with key = (Row, Col) val = Char
createMazeMap :: FilePath -> IO MazeMap
createMazeMap filePath = do
  -- first extract file contents into [[Char]]
  rows <- lines <$> readFile filePath

  let gridHeight = length rows
  let gridWidth = length (head rows)
  -- create a list of tuples from (0,0) to (gridHeight, gridWidth)
  let tuples = [(i, j) | i <- [0 .. gridHeight - 1], j <- [0 .. gridWidth - 1]]
  -- flatten grid that I've read in and zip it with my list of tuples
  let flatGrid = concat rows

  let keyValPairs = zip tuples flatGrid

  -- fill in my MazeMap with this list of [(Int, Int), Char]
  let maze = Map.fromList keyValPairs

  pure maze

----------------------------------------------------------------------------------

----------------------------------------------------------------------------------
-- Looking up path --
----------------------------------------------------------------------------------

-- given Char, return the position (helper for finding S, F only**)
findStartandEndPos :: MazeMap -> (Pos, Pos)
findStartandEndPos maze = do
  -- collapse Map into a list of tuples
  let mazeList = Map.toList maze
  -- search for the Pos for 'S' and  Pos for 'F'
  let startPos = fst (head (filter (\(_, y) -> y == 'S') mazeList))
  let endPos = fst (head (filter (\(_, y) -> y == 'F') mazeList))
  -- return them as a tuple
  (startPos, endPos)

-- use nextPos helper to construct list of 4 next possible moves
nextPossiblePositions :: Pos -> MazeMap -> Path -> [Maybe Pos]
nextPossiblePositions (currRow, currCol) maze memory = do
  let down = nextPos (currRow + 1, currCol) maze memory
  let right = nextPos (currRow, currCol + 1) maze memory
  let up = nextPos (currRow - 1, currCol) maze memory
  let left = nextPos (currRow, currCol - 1) maze memory
  [down, right, up, left]

-- looks up the given Pos in map and checks if it is valid
-- i.e., it exists, is not already visited & not an 'X'
nextPos :: Pos -> MazeMap -> Path -> Maybe Pos
nextPos (r, c) maze memory =
  -- first check if we've already visited this position
  if (r, c) `elem` memory
    then Nothing
    else -- check if position exists and free to move to
    case Map.lookup (r, c) maze of
      Nothing -> Nothing
      Just p -> if p == 'X' then Nothing else Just (r, c)

-- return a path, if one exists, given a starting Pos and MazeMap
searchMaze :: Maybe Pos -> MazeMap -> Path -> Maybe Path
searchMaze Nothing _ _ = Nothing
searchMaze (Just currPos) maze memory = do
  -- first get the start Pos and end Pos from map
  let (_, endPos) = findStartandEndPos maze

  -- get the 4 next possible positions from currPos
  let (currRow, currCol) = currPos
  let nextMoves = nextPossiblePositions (currRow, currCol) maze memory

  -- if any of the next possible moves leads to 'F' then pick that
  if Just endPos `elem` nextMoves
    then Just (memory ++ [currPos] ++ [endPos])
    else
      do
        -- recursively search for full paths in each direction
        -- and return the first path that is not Nothing
        searchMaze (head nextMoves) maze (memory ++ [currPos])
        <|> searchMaze (nextMoves !! 1) maze (memory ++ [currPos])
        <|> searchMaze (nextMoves !! 2) maze (memory ++ [currPos])
        <|> searchMaze (nextMoves !! 3) maze (memory ++ [currPos])

----------------------------------------------------------------------------------
-- Extra Credit --
----------------------------------------------------------------------------------
-- helper from a Stack post:
groupTo :: Int -> [a] -> [[a]]
groupTo _ [] = []
groupTo n l
  | n > 0 = take n l : groupTo n (drop n l)
  | otherwise = error "Negative or zero n"

-- helper to update the ith index in a list
-- helper from a Stack post:
replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x : xs)
  | n == 0 = newVal : xs
  | otherwise = x : replaceNth (n - 1) newVal xs

-- helper from a stack post to add a space after
-- every char in a string
spaceN :: Int -> String -> String
spaceN n = init . go
  where
    go [] = []
    go xs = let (as, bs) = splitAt n xs in as ++ (' ' : go bs)

---------------------------------------------------------------------------------------
-- TODO --

-- helper to format the board (adds space b/w characters)
-- and shifts the maze more towards the middle
formatMaze :: [[Char]] -> [[Char]]
formatMaze rowOfVals = do
  -- first append "                " to each string
  let shiftedRowOfVals = (\(i) -> ("     " : i)) rowOfVals
  -- let spacedandShiftedRowOfVals = map (spaceN 1) shiftedRowOfVals
  -- then add an extra space between each character in each string
  -- spacedandShiftedRowOfVals
  shiftedRowOfVals

---------------------------------------------------------------------------------------

printEmptyMaze :: [((Row, Col), Char)] -> IO ()
printEmptyMaze mazeList = do
  -- find height of maze
  let mazeWidth = maximum (map (snd . fst) mazeList) + 1
  -- extract the vals from the maze map
  let mazeVals = map snd mazeList
  -- mazeVals groupBy height of grid to group the mazeVals
  let rowOfVals = groupTo mazeWidth mazeVals
  -- print each row onto a separate line
  putStrLn ""
  putStr "-------------- Maze -------------"
  putStrLn ""
  putStrLn ""
  putStr (unlines rowOfVals)
  putStrLn ""
  putStrLn ""

printMazeWithPos :: Maybe Pos -> [((Row, Col), Char)] -> IO ()
printMazeWithPos Nothing mazeList = printEmptyMaze mazeList
printMazeWithPos (Just pose) mazeList = do
  -- find width & height of maze
  let mazeWidth = maximum (map (snd . fst) mazeList) + 1
  let updatedMazeVals = updateMazeVals (Just pose) mazeList
  -- TESTING FORMATTER: 
  let rowOfVals = formatMaze (groupTo mazeWidth updatedMazeVals)
  -- let rowOfVals = groupTo mazeWidth updatedMazeVals
  putStrLn "---------------------------------"
  putStrLn ""
  putStr (unlines rowOfVals)
  putStrLn ""

-- update MazeList with the new character on the maze
updateMazeVals :: Maybe Pos -> [((Row, Col), Char)] -> [Char]
updateMazeVals Nothing _ = []
updateMazeVals (Just pose) mazeList = do
  let mazeWidth = maximum (map (snd . fst) mazeList) + 1
  -- extract the vals from the maze map
  let mazeVals = map snd mazeList
  -- find index of val to change given its Pos
  let i = (mazeWidth * fst pose) + snd pose
  -- create new list of mazeVals that's updated at i
  replaceNth i '★' mazeVals

printMazewithPath :: Maybe Path -> [((Row, Col), Char)] -> IO ()
printMazewithPath Nothing mazeList = printEmptyMaze mazeList
printMazewithPath (Just path) mazeList =
  case path of
    [] -> putStrLn "Maze complete!"
    [pos] -> printMazeWithPos (Just pos) mazeList
    (pos : rest) -> do
      printMazeWithPos (Just pos) mazeList
      -- create the updated list of [char] to print after adding '★'
      let newMazeVals = updateMazeVals (Just pos) mazeList
      -- extract the (row, cols) from mazeList
      let keyValPairs = map fst mazeList
      -- replace all [char] in [(row, col), char] with the updated [char']
      let newMazeList = zip keyValPairs newMazeVals
      printMazewithPath (Just rest) newMazeList

main :: IO ()
main = do
  input <- getArgs

  case input of
    [rf] -> do
      maze <- createMazeMap rf
      let startPos = fst $ findStartandEndPos maze
      let maybePath = searchMaze (Just startPos) maze []
      case maybePath of
        Nothing -> putStrLn "unsolvable maze"
        Just p -> do
          putStrLn "---------------------------------"
          putStrLn "Matrix solved!"
          putStr "Path: "
          print p
    (rf : ["animate"]) -> do
      maze <- createMazeMap rf
      let mazeList = Map.toList maze
      let startPos = fst $ findStartandEndPos maze
      let maybePath = searchMaze (Just startPos) maze []
      printEmptyMaze mazeList
      -- printMazeWithPos (Just (1, 1)) mazeList
      case maybePath of
        Nothing -> putStrLn "unsolvable maze"
        Just p -> do
          printMazewithPath (Just p) mazeList
          putStrLn "---------------------------------"
          putStrLn "---------------------------------"
          putStrLn "Matrix solved!"
          putStr "Path: "
          print p
    _ -> do
      putStrLn "Please enter a filePath that contains a maze."

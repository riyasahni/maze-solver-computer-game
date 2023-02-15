module MazeSolver where

import Control.Applicative
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set.Lens

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
  | n > 0 = (take n l) : (groupTo n (drop n l))
  | otherwise = error "Negative or zero n"

printEmptyMaze :: MazeMap -> IO ()
printEmptyMaze maze = do
  let mazeList = Map.toList maze

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

printMazeWithPos :: Maybe Pos -> MazeMap -> IO ()
printMazeWithPos Nothing maze = printEmptyMaze maze
printMazeWithPos (Just pose) maze = do
  let mazeList = Map.toList maze

  -- find width & height of maze
  let mazeWidth = maximum (map (snd . fst) mazeList) + 1
  let mazeHeight = maximum (map (fst . fst) mazeList) + 1
  -- extract the vals from the maze map
  let mazeVals = map snd mazeList
  -- find index of val to change given its Pos
  let i = mazeWidth * (fst pose + 1) + snd pose
  -- create new list of mazeVals that's updated at i
  let updatedMazeVals = Tree (element i) '*' mazeVals
  -- mazeVals groupBy width of grid to group the mazeVals
  let rowOfVals = groupTo mazeWidth mazeVals
  -----------------------------------
  -- TODO --
  -- update the maze by adding '*' on covered ** JUST FIGURE OUT HOW
  -- TO UPDATE THE MAZEVALS WITH NEW VAL AT INDEX 'i' ....
  -----------------------------------
  -- print each row onto a separate line
  -- print each row onto a separate line
  putStrLn ""
  putStr "-------------- Maze -------------"
  putStrLn ""
  putStrLn ""
  putStr (unlines rowOfVals)
  putStrLn ""
  putStrLn ""

  -- unlines

  ---------------------------------------------------------------------------------
  -- TODO HERE:
  -- key-val lookup where 'S' is in the loaded map, then call searchMaze (startRow, startCol) (createMazeMap "mazeFile.txt") []
  -- error "TODO"

  -- use Map.member to determine whether the map has a 'S' and 'F' to begin with...
  -- getArgs
  -- then pattern match
  ---------------------------------------------------------------------------------

  putStrLn ""
  putStr "-------------- Maze -------------"
  putStrLn ""
  putStrLn ""
  putStr (unlines rowOfVals)
  putStrLn ""
  putStrLn ""

printMazewithPath :: Maybe Path -> MazeMap -> IO ()
printMazewithPath Nothing maze = printEmptyMaze maze
printMazewithPath (Just path) maze = do
  pure undefined

main :: IO ()
main = do
  maze <- createMazeMap "mazes/maze-03.txt"

  printEmptyMaze maze

  let startPos = fst $ findStartandEndPos maze
  let maybePath = searchMaze (Just startPos) maze []

  case maybePath of
    Nothing -> putStrLn "unsolvable maze"
    Just p -> do
      putStrLn "---------------------------------"
      print p
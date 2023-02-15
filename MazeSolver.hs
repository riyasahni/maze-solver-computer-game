module MazeSolver where

import Control.Applicative
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe

type Path = [Pos]

type Pos = (Row, Col)

type Row = Int

type Col = Int

type MazeMap = Map Pos Char

----------------------------------------------------------------------------------
-- Loading data --
----------------------------------------------------------------------------------

-- load data from file into a Map with key = (Row, Col) val = Char
createMazeMap :: IO MazeMap
createMazeMap = do
  -- first extract file contents into [[Char]]
  rows <- lines <$> readFile "mazeFile.txt"

  let gridHeight = length rows
  let gridWidth = length (head rows)
  -- create a list of tuples from (0,0) to (gridHeight, gridWidth)
  let tuples = [(i, j) | i <- [0 .. gridHeight - 1], j <- [0, gridWidth - 1]]
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
searchMaze :: Pos -> MazeMap -> Path -> Maybe Path
searchMaze (currRow, currCol) maze memory = do
  -- first get the start Pos and end Pos from map
  let (startPos, endPos) = findStartandEndPos maze

  -- get the 4 next possible positions from currPos
  let nextMoves = nextPossiblePositions (currRow, currCol) maze memory

  -- if any of the next possible moves leads to 'F' then pick that
  if Just endPos `elem` nextMoves
    then Just (memory ++ [endPos])
    else do
      -- otherwise see which next valid move to make
      let nextPos =
            head nextMoves
              <|> nextMoves !! 1
              <|> nextMoves !! 2
              <|> nextMoves !! 3

      if isNothing nextPos
        then Nothing
        else do
          -- add nextPos to memory and call searchMaze from nextPos

          {-  -- base case: if we reach F then return path
           if (nextRow, nextCol) == endPos
             then Just (memory : endPos)
             else -- base case: if we're going in circles return Nothing

               if (nextRow, nextCol) `elem` memory
                 then Nothing
                 else do
                   case Map.lookup (nextRow + 1, nextCol) maze of {} -}

          pure undefined

{-   case Map.lookup 'F' maze of {}
  --

  if -- first check if lookup currPos == 'F'
  -- next check if we've already visited currPos
  (currRow, currCol) `elem` memory
    then Nothing
    else -- otherwise, recursively determine path
    case Map.lookup (currRow, currCol) maze of
      Nothing -> error "Given position is not in maze"
      Just val -> searchMaze () -}

{-
let nextPos = searchMaze (currRow+1, currCol) maze memory <|>
              searchMaze (currRow, currCol+1) maze memory <|>
              searchMaze (currRow)

-}

-- key-val lookup where 'S' is in the loaded map, then call searchMaze (startRow, startCol) (createMazeMap "mazeFile.txt") []

main :: IO ()
main =
  -- use Map.member to determine whether the map has a 'S' and 'F' to begin with...

  --      something nice to use:
  --      'alternative' is part of control.Applicative: <|>
  --      Just 5 <|> Nothing returns Just 5
  --      if the left side is 'Nothing', then it will just return the right regardless of what the right value is...
  --      I CAN DO MY SEARCH AND RETURN WHETHER I SHOULD MOVE ROW-WISE OR COLUMN-WISE, OR WHETHER THE MAZE HAS NO SOLUTION (in which case it will return Nothing)

  --      I can use <|> and recursion to figure out a path without having to actively backtrack...
  --      i.e., if I was at (2,2) (check out all 4 directoins)
  --      pos @ (r, c)
  --      if pos 'elem' memory then Nothing else:
  --          search (3,2) map memory <|> search (2,3) map memory <|> search (2, 1) map memory <|> search (reverse (head memory)) map memory

  --      make decision from <|> that isn't already in memory, if possible. make a list of all the neighbors and check if I can make
  --      a valid move on any of the neighbors (i.e., it isn't already in Memory and it isn't an 'X')

  error "TODO"

-- can load the grid into a Map type and then do lookups on each element to find the S and F
-- key = (row, col) and val = the Char

-- when I find S, start appending each subsequent non-empty key-value pair into the
-- [(int,int)] that I will eventually return... until I hit F
-- conduct lookups by row first, then by col if row is blocked

-- if I hit F first, then do the same until I find S, then return the reversed list

-- elemIndices --> do a Key lookup (runs in linear time)
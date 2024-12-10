{-# LANGUAGE RankNTypes #-}

import Control.Comonad
import Control.Comonad.Store
import Control.Monad (forM_)
import Data.Array.Unboxed

-- This example:
-- Uses Store (Int, Int) a as the comonadic context, where (Int, Int) are coordinates and a is the cell state (Bool for alive/dead).
-- Wraps a finite 2D array (UArray (Int, Int) Bool) and uses it to define the store function.
-- Implements extend and extract using the standard Comonad instance for Store.
-- Defines a lifeRule function that implements Conway’s rules by looking up neighbors via peek.
-- Performs a single iteration by using extend lifeRule on the Grid.

-- Our Grid type is a Store comonad parameterized by (Int,Int) for indexing and Bool for cell state.
type Grid = Store (Int, Int) Bool

-- Dimensions
width, height :: Int
width = 5
height = 5

-- | Check if a coordinate is within the array bounds
inRangeBounds :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
inRangeBounds ((r0, c0), (r1, c1)) (r, c) =
  r0 <= r && r <= r1 && c0 <= c && c <= c1

-- | Access function for the array. Outside cells are False (dead)
getCell :: UArray (Int, Int) Bool -> (Int, Int) -> Bool
getCell arr ix
  | inRangeBounds (bounds arr) ix = arr ! ix
  | otherwise = False -- outside the array is dead

-- | Create a Grid from an array and an initial focus point
mkGrid :: UArray (Int, Int) Bool -> (Int, Int) -> Grid
mkGrid arr start = store (getCell arr) start

-- rules
-- We define the standard Game of Life transition rule for a single cell. To do this with the Store comonad, we:
-- - Use extract w to get the current cell’s state (alive/dead).
-- - Use peek to read the state of all 8 neighbors.
-- - Apply the standard life rule to determine the next state of the current cell.
neighborCoords :: [(Int, Int)]
neighborCoords =
  [ (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1)
  ]

-- | Count how many of the 8 neighbors are alive
countAliveNeighbors w = length $ filter id $ map (\(dr, dc) -> peek (shift dr dc (pos w)) w) neighborCoords
  where
    shift dr dc (r, c) = (r + dr, c + dc)

-- | Game of Life rule:
--   - If alive and neighbors == 2 or 3, stays alive.
--   - If dead and neighbors == 3, becomes alive.
--   - Otherwise, dies.
lifeRule :: Grid -> Bool
lifeRule w =
  let alive = extract w
      n = countAliveNeighbors w
   in if alive
        then n == 2 || n == 3
        else n == 3

-- evolve the grid
nextGeneration :: Grid -> Grid
nextGeneration = extend lifeRule

-- usage:

-- Sample initial pattern: a vertical line (blinker) in the middle row
initialArray :: UArray (Int, Int) Bool
initialArray =
  array
    ((0, 0), (height - 1, width - 1))
    [((r, c), isAlive r c) | r <- [0 .. height - 1], c <- [0 .. width - 1]]
  where
    isAlive r c = (r, c) `elem` [(2, 1), (2, 2), (2, 3)] -- a blinker at row 2

-- Print the grid to the console
printGrid :: Grid -> IO ()
printGrid g = do
  let ((r0, c0), (r1, c1)) = ((0, 0), (height - 1, width - 1))
  forM_ [r0 .. r1] $ \r -> do
    forM_ [c0 .. c1] $ \c ->
      putStr (if peek (r, c) g then "■ " else ". ")
    putStrLn ""

main :: IO ()
main = do
  let g = mkGrid initialArray (2, 2) -- focus on the center cell
  putStrLn "Initial:"
  printGrid g
  let g' = nextGeneration g
  putStrLn "Next:"
  printGrid g'

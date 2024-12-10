import Control.Applicative (liftA2)
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed

-- cabal install --lib array

-- Dimensions of the grid
width, height :: Int
width = 5
height = 5

-- Index type for convenience
type Coord = (Int, Int)

-- Initialize a sample board
-- For example, a small blinker pattern in the center of a 5x5 grid
initialBoard :: UArray Coord Bool
initialBoard = runSTUArray $ do
  arr <- newArray ((0, 0), (height - 1, width - 1)) False
  writeArray arr (2, 1) True
  writeArray arr (2, 2) True
  writeArray arr (2, 3) True
  return arr

-- Count the neighbors of a cell
-- We'll use Applicative style to combine reads from multiple positions
countNeighbors :: STUArray s Coord Bool -> Coord -> ST s Int
countNeighbors arr (r, c) = do
  let neighbors =
        [ (r - 1, c - 1), -- top left
          (r - 1, c), -- top
          (r - 1, c + 1), -- top right
          (r, c - 1), -- left
          (r, c + 1), -- right
          (r + 1, c - 1), -- bottom left
          (r + 1, c), -- bottom
          (r + 1, c + 1) -- bottom right
        ]

  -- A helper function that returns 1 if True else 0
  let cellToInt pos = do
        inBounds <- inRangeM arr pos
        if inBounds
          then fmap (\alive -> if alive then 1 else 0) (readArray arr pos)
          else pure 0

  -- Use traverse and sum the results with Applicative combinators
  sum <$> traverse cellToInt neighbors

-- Check if a coordinate is within array bounds
inRangeM :: STUArray s Coord Bool -> Coord -> ST s Bool
inRangeM arr (r, c) = do
  ((r0, c0), (r1, c1)) <- getBounds arr
  return $ r0 <= r && r <= r1 && c0 <= c && c <= c1

-- Game of Life rules:
-- - A live cell with 2 or 3 neighbors survives.
-- - A dead cell with exactly 3 neighbors becomes alive.
-- - Otherwise the cell dies or remains dead.
nextState :: Bool -> Int -> Bool
nextState True n = n == 2 || n == 3
nextState False n = n == 3

-- Perform one iteration of the Game of Life
step :: UArray Coord Bool -> UArray Coord Bool
step arr = runSTUArray $ do
  stArr <- thaw arr -- Convert the immutable array to mutable
  ((r0, c0), (r1, c1)) <- getBounds stArr
  newArr <- newArray ((r0, c0), (r1, c1)) False

  -- We'll iterate over all cells and calculate their next state.
  -- Notice how we can use Applicative style (e.g., liftA2) to combine computations
  -- if needed. In this simple form, we just do reads and apply rules.
  forM_ [r0 .. r1] $ \r ->
    forM_ [c0 .. c1] $ \c -> do
      neighborsCount <- countNeighbors stArr (r, c)
      oldVal <- readArray stArr (r, c)
      writeArray newArr (r, c) (nextState oldVal neighborsCount)

  return newArr

-- Helper function to loop over ranges
forM_ :: (Monad m) => [a] -> (a -> m b) -> m ()
forM_ = flip mapM_

-- Example usage:
-- Running `main` will show the initial and the next state in a simple textual form.
main :: IO ()
main = do
  let initial = initialBoard
  putStrLn "Initial:"
  printBoard initial
  let next = step initial
  putStrLn "Next:"
  printBoard next

printBoard :: UArray Coord Bool -> IO ()
printBoard arr = do
  let ((r0, c0), (r1, c1)) = bounds arr
  forM_ [r0 .. r1] $ \r -> do
    forM_ [c0 .. c1] $ \c ->
      putStr (if arr ! (r, c) then "■ " else ". ")
    putStrLn ""

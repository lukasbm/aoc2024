import Control.Monad.ST
import Data.Array.IArray (Array, elems)
import Data.Array.ST

type Cell = Bool -- or some other type representing cell state

type Coord = (Int, Int)

simulateCA :: Int -> Int -> Int -> (Coord -> Cell) -> Array Coord Cell
simulateCA width height steps initial = runST $ do
  -- Create a mutable array for the initial state
  arr <- newArray ((0, 0), (width - 1, height - 1)) False :: ST s (STArray s Coord Cell)

  -- Initialize the array with initial conditions
  mapM_
    (\(x, y) -> writeArray arr (x, y) (initial (x, y)))
    [(x, y) | x <- [0 .. width - 1], y <- [0 .. height - 1]]

  -- Run the simulation for the given number of steps
  replicateM_ steps (stepCA arr width height)

  -- Freeze the array to produce an immutable result
  freeze arr

stepCA :: STArray s Coord Cell -> Int -> Int -> ST s ()
stepCA oldArr width height = do
  newArr <- newArray ((0, 0), (width - 1, height - 1)) False

  -- Compute next generation
  forM_ [0 .. width - 1] $ \x ->
    forM_ [0 .. height - 1] $ \y -> do
      oldVal <- readArray oldArr (x, y)
      neighbors <- countNeighbors oldArr (x, y) width height
      let newVal = rule oldVal neighbors
      writeArray newArr (x, y) newVal

  -- Now copy newArr into oldArr
  forM_ [0 .. width - 1] $ \x ->
    forM_ [0 .. height - 1] $ \y -> do
      val <- readArray newArr (x, y)
      writeArray oldArr (x, y) val

rule :: Cell -> Int -> Cell
rule oldVal neighborCount =
  -- For example, Conway’s Game of Life rules:
  if oldVal
    then neighborCount == 2 || neighborCount == 3
    else neighborCount == 3

countNeighbors :: STArray s Coord Cell -> Coord -> Int -> Int -> ST s Int
countNeighbors arr (x, y) width height = do
  let neighbors =
        [ (x + dx, y + dy)
          | dx <- [-1 .. 1],
            dy <- [-1 .. 1],
            not (dx == 0 && dy == 0),
            x + dx >= 0,
            x + dx < width,
            y + dy >= 0,
            y + dy < height
        ]
  countTrues neighbors
  where
    countTrues =
      foldM
        ( \acc pos -> do
            val <- readArray arr pos
            return $ acc + if val then 1 else 0
        )
        0

main :: IO ()
main = do
  let width = 100
      height = 100
      steps = 50
      initial (x, y) = False -- define your initial pattern here
  let finalState = simulateCA width height steps initial
  print (elems finalState) -- or display it in some format

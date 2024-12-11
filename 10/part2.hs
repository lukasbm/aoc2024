import Data.Array.IArray
import Data.Array.Unboxed
import Data.List (nub)
import System.Environment (getArgs)

type Coord = (Int, Int)

type Grid = UArray Coord Int

-- Convert 2D list to UArray
toUArray :: [[Int]] -> Grid
toUArray xs = array bounds elements
  where
    rows = length xs
    cols = if null xs then 0 else length (head xs)
    bounds = ((1, 1), (rows, cols)) -- Assuming 1-based indexing
    elements =
      [ ((i, j), xs !! (i - 1) !! (j - 1))
        | i <- [1 .. rows],
          j <- [1 .. cols]
      ]

findIndicesByValue2D :: (Eq a, IArray UArray a) => a -> UArray (Int, Int) a -> [(Int, Int)]
findIndicesByValue2D value arr = [idx | (idx, val) <- assocs arr, val == value]

getNeighbors :: Grid -> Coord -> [Coord]
getNeighbors grid (i, j) =
  let ((minI, minJ), (maxI, maxJ)) = bounds grid
      potentialNeighbors = [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]
   in filter (\(x, y) -> x >= minI && x <= maxI && y >= minJ && y <= maxJ) potentialNeighbors

-- trailhead always at height 0
-- grid -> start/curr -> destinations
findPath :: Grid -> Coord -> [Coord]
findPath grid pos =
  let validMoves = filter (\x -> grid ! x == grid ! pos + 1) $ getNeighbors grid pos
   in if grid ! pos == 9 then [pos] else concatMap (findPath grid) validMoves

main = do
  args <- getArgs
  raw_text <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let grid = toUArray $ (map . map) (read . pure :: Char -> Int) $ lines raw_text :: Grid

  -- get trailheads
  -- print $ findIndicesByValue2D 0 $ toUArray grid

  -- print $ grid ! (1, 4)
  -- print $ getNeighbors grid (1, 4)
  -- print $ findPath grid (1, 4)

  let trailheads = findIndicesByValue2D 0 grid
  -- print $ length trailheads

  -- print $ map (findPath grid) trailheads

  -- count unique destinations per trailhead and sum up
  print $ sum $ map (length . nub . findPath grid) trailheads

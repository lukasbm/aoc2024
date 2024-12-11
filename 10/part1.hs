import Data.Array.IArray
import Data.Array.Unboxed
import System.Environment (getArgs)

type Grid = UArray (Int, Int) Int

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

-- trailhead always at height 0

main = do
  args <- getArgs
  raw_text <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let grid = (map . map) (read . pure :: Char -> Int) $ lines raw_text :: [[Int]]
  print $ toUArray grid

import Data.Array (Array, array, range)
import Debug.Trace (trace)
import System.Environment (getArgs)

data Cell = Free | Robots Int deriving (Eq, Ord)

instance Show Cell where
  show Free = "."
  show (Robots a) = show a

type Coord = (Int, Int)

type Grid = Array Coord Cell

parseCell :: Char -> Cell
parseCell '.' = Free
parseCell c = Robots (read [c])

toArray :: [[a]] -> Array Coord a
toArray xss =
  let rows = length xss
      cols = if null xss then 0 else length (head xss)
      bounds = ((0, 0), (rows - 1, cols - 1))
      elems = concat xss
   in array bounds (zip (range bounds) elems)

main = do
  args <- getArgs
  raw <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let grid = (map . map) parseCell (lines raw)
  print $ toArray $ grid

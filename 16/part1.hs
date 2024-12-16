import Data.Array
import System.Environment (getArgs)

data Cell = Start | End | Wall | Free deriving (Eq)

instance Show Cell where
  show Start = "S"
  show End = "E"
  show Wall = "#"
  show Free = "."

parseCell :: Char -> Cell
parseCell '.' = Free
parseCell '#' = Wall
parseCell 'S' = Start
parseCell 'E' = End

type Coord = (Int, Int)

type Grid = Array Coord Cell

-- starting with coords from top left
parseGrid :: [[Char]] -> Grid
parseGrid c =
  let height = length c
      width = length $ head c
   in listArray ((0, 0), (height - 1, width - 1)) $ map parseCell (concat c)

prettyPrint :: Array (Int, Int) Cell -> String
prettyPrint arr =
  let ((rowStart, colStart), (rowEnd, colEnd)) = bounds arr
      rows = [[arr ! (r, c) | c <- [colStart .. colEnd]] | r <- [rowStart .. rowEnd]]
   in unlines $ map (concatMap ((: []) . head . show)) rows

main = do
  args <- getArgs
  raw <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let grid = parseGrid (lines raw)
  print grid

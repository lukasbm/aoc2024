{-# LANGUAGE MonoLocalBinds #-}

-- cabal install --lib heaps

import Data.Array
import Data.Array.IArray (IArray)
import Data.Graph
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

findIndicesByValue2D :: (Eq a, IArray Array a) => a -> Array Coord a -> [Coord]
findIndicesByValue2D value arr = [idx | (idx, val) <- assocs arr, val == value]

data Direction = Up | Down | Left | Right deriving (Show, Eq)

-- in practice we will use Coord for a
type Graph = Map a [(a, Int)]

-- essentially need DFS to turn grid into graph
toGraph :: Grid -> Graph
toGraph arr =
  let graph_bounds = snd $ bounds arr
      start = head $ findIndicesByValue2D Start arr
      end = head $ findIndicesByValue2D End arr
   in go Right start []
  where
    vertexId :: Coord -> Int
    vertexId (x, y) = 1000 * x + y

    go :: Direction -> Coord -> [Coord] -> Graph 
    go dir pos visited = 

-- TODO:
-- best approach seems to be to parse it into a graph,
-- then use dijsktra (we don't want nor have heuristics)
main = do
  args <- getArgs
  raw <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let grid = parseGrid (lines raw)
  putStrLn $ prettyPrint grid

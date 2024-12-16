{-# LANGUAGE MonoLocalBinds #-}

-- cabal install --lib heaps

import Control.Monad.ST
import Data.Array
import Data.Array.IArray (IArray)
import Data.Array.ST (STArray)
import System.Environment (getArgs)
import Prelude hiding (Left, Right)

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

data Direction = Up | Right | Down | Left deriving (Show, Eq, Enum)

type Graph = Array Node [(Node, Int)]

type Node = (Coord, Direction)

type Move = (Node, Node, Int)

-- essentially need DFS to turn grid into graph
toGraph :: Grid -> Graph
toGraph arr = go Right start []
  where
    graph_bounds = snd $ bounds arr
    start = head $ findIndicesByValue2D Start arr
    end = head $ findIndicesByValue2D End arr

    -- gets all valid neighbors
    -- FIXME: restrict to 90 deg turns
    validMoves :: [Move] -> Move
    validNeighbors pos dir = filter (\x -> arr ! x == Free) [moveTo pos dir Up, moveTo pos dir Down, moveTo pos dir Left, moveTo pos dir Right]

    toGraph :: [Move] -> Graph
    toGraph moves = let bounds = asdasd in listArray bounds bounds

    rotationCost :: Direction -> Direction
    -- from left
    rotationCost Left Left = 0
    rotationCost Left Right = 2000
    rotationCost Left Up = 1000
    rotationCost Left Down = 1000
    -- from right
    rotationCost Right Left = 2000
    rotationCost Right Right = 0
    rotationCost Right Up = 1000
    rotationCost Right Down = 1000
    -- from up
    rotationCost Up Left = 1000
    rotationCost Up Right = 1000
    rotationCost Up Up = 0
    rotationCost Up Down = 2000
    -- from down
    rotationCost Down Left = 1000
    rotationCost Down Right = 1000
    rotationCost Down Up = 2000
    rotationCost Down Down = 0

    -- current pos, current dir, target move -> (new pos, cost)
    -- we simply disallow 180° turns, as they wont be needed!
    moveTo :: Node -> Direction -> Move
    moveTo (from@(y, x), Left) dir = let to = (y, x - 1) in if dir == Left then (from, to, 1) else (to, 1001)
    moveTo (from@(y, x), Right) dir = let to = (y, x - 1) in if dir == Right then (from, to, 1) else (to, 1001)
    moveTo (from@(y, x), Up) dir = let to = (y, x - 1) in if dir == Up then (from, to, 1) else (to, 1001)
    moveTo (from@(y, x), Down) dir = let to = (y, x - 1) in if dir == Down then (from, to, 1) else (to, 1001)

    adjList :: Graph
    adjList = toGraph $ dfs start [] $ array
      where
        dfs :: Node -> [Node] -> [Move]
        dfs node@(pos, dir) visited g =
          let neighs = validNeighbors
           in if arr ! pos == Wall then [] else node : foldAlong (\acc visited next_node -> acc ++ dfs next_node visited) (++) [] neighs (neighs ++ [node] ++ visited)

-- best approach seems to be to parse it into a graph,
-- then use dijsktra (we don't want nor have heuristics)
main = do
  args <- getArgs
  raw <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let grid = parseGrid (lines raw)
  putStrLn $ prettyPrint grid

-- stolen from rosettacode lol

dijkstra :: (Ix v, Num w, Ord w, Bounded w) => v -> v -> Array v [(v, w)] -> (Array v w, Array v v)
dijkstra src invalid_index adj_list = runST $ do
  min_distance <- newSTArray b maxBound
  writeArray min_distance src 0
  previous <- newSTArray b invalid_index
  let aux vertex_queue =
        case S.minView vertex_queue of
          Nothing -> return ()
          Just ((dist, u), vertex_queue') ->
            let edges = adj_list ! u
                f vertex_queue (v, weight) = do
                  let dist_thru_u = dist + weight
                  old_dist <- readArray min_distance v
                  if dist_thru_u >= old_dist
                    then
                      return vertex_queue
                    else do
                      let vertex_queue' = S.delete (old_dist, v) vertex_queue
                      writeArray min_distance v dist_thru_u
                      writeArray previous v u
                      return $ S.insert (dist_thru_u, v) vertex_queue'
             in foldM f vertex_queue' edges >>= aux -- note that aux is being called within its own definition (i.e. aux is recursive). The foldM only iterates on the neighbours of v, it does not execute the while loop itself in Dijkstra's
  aux (S.singleton (0, src))
  m <- freeze min_distance
  p <- freeze previous
  return (m, p)
  where
    b = bounds adj_list
    newSTArray :: (Ix i) => (i, i) -> e -> ST s (STArray s i e)
    newSTArray = newArray

shortestPathTo :: (Ix v) => v -> v -> Array v v -> [v]
shortestPathTo target invalid_index previous =
  aux target []
  where
    aux vertex acc
      | vertex == invalid_index = acc
      | otherwise = aux (previous ! vertex) (vertex : acc)

-- a = Coord
-- result accumulator: b = [(Coord, Int)]
-- helper c = [Coord] (visited)
-- the second function is essentially another helper to modify b
-- example usage:
-- print $ foldAlong (\acc helper x -> if x `elem` helper then acc else acc + x) const 0 [1 .. 5] [3]
-- add ups 1 to 5 except 3
-- very contrived example
foldAlong :: (b -> c -> a -> b) -> (c -> b -> c) -> b -> [a] -> c -> b
foldAlong _ _ acc [] _ = acc
foldAlong f h acc (x : xs) cs =
  let y = f acc cs x
   in foldAlong f h y xs (h cs y)

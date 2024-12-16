import Data.Array (Array, array, bounds, listArray, range)
import Data.Array.IArray hiding (array, bounds, listArray)
import Data.Char (isDigit)
import Data.List (group)
import Debug.Trace (trace)
import System.Environment (getArgs)

type Coord = (Int, Int)

data Robot = Robot {pos :: Coord, vel :: Coord} deriving (Eq, Show)

data Quadrant = TopLeft | TopRight | BottomLeft | BottomRight | None deriving (Eq, Show)

b2i :: Bool -> Int
b2i False = 0
b2i True = 1

atoi :: String -> Int
atoi "" = error "empty string"
atoi x = read $ filter (\x -> isDigit x || x == '-') x

parseRobot :: String -> Robot
parseRobot r =
  let (p, v) = break (== ' ') r
      (px, py) = break (== ',') p
      (vx, vy) = break (== ',') v
   in Robot (atoi px, atoi py) (atoi vx, atoi vy)

-- we index (x,y), where x is number of tile from the left wall and y is number of rows from the top!
-- positive x means moving right, positive y means moving down
-- grid is 0 indexed!

visualizeRobots :: Coord -> [Robot] -> Array Coord Bool
visualizeRobots (width, height) robots = array ((0, 0), (width - 1, height - 1)) [((x, y), (x, y) `elem` map pos robots) | x <- [0 .. width - 1], y <- [0 .. height - 1]]

-- | 'slidingWindow2' applies a function to every subgrid of a given size within a grid.
-- The first parameter is the size of the subgrid (number of rows and columns).
-- The second parameter is the function to apply to each subgrid.
-- The third parameter is the array to process.
slidingWindowArray2 :: Coord -> ([a] -> b) -> Array Coord a -> [b]
slidingWindowArray2 size@(nrow, ncol) func arr = [func $ neighbors (i, j) | i <- [0 .. width], j <- [0 .. height]]
  where
    ((_, _), (width, height)) = bounds arr
    neighbors (i, j) = [val | (idx, val) <- assocs arr, idx < (i + width, j + height) && idx >= (i, j)]

-- https://en.wikipedia.org/wiki/Indicators_of_spatial_association
chaos :: Array Coord Bool -> Int
chaos arr = sum $ slidingWindowArray2 (4, 4) windowChaos arr
  where
    windowChaos a = sum $ map b2i a

main = do
  args <- getArgs
  raw <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let size = if head args == "input.txt" then (101, 103) else (11, 7) :: Coord
  let robots = map parseRobot (lines raw)
  let robots_moved = iterate (step size) robots
  let robots_moved_grid = map (visualizeRobots size) robots_moved
  putStrLn $
    concatMap (\(i, pp, ch) -> "chaos after steps" <> show i <> ": " <> show ch <> "\n" <> pp) $
      filter (\(_, _, ch) -> ch > 0) (zip3 [0 ..] (map prettyPrint robots_moved_grid) (map chaos robots_moved_grid))

-- moves the robot according to its own velocity
-- will wrap around the grid if border is reached!
moveRobot :: Coord -> Robot -> Robot
moveRobot (width, height) (Robot (px, py) (vx, vy)) = Robot ((px + vx) `mod` width, (py + vy) `mod` height) (vx, vy)

step :: Coord -> [Robot] -> [Robot]
step size = map (moveRobot size)

prettyPrint :: Array (Int, Int) Bool -> String
prettyPrint arr =
  let ((rowStart, colStart), (rowEnd, colEnd)) = bounds arr
      rows = [[if arr ! (r, c) then 'X' else '.' | c <- [colStart .. colEnd]] | r <- [rowStart .. rowEnd]]
   in unlines $ map (unwords . map (: [])) rows

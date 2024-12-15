import Data.Array (Array, array, bounds, listArray, range)
import Data.Array.IArray hiding (array, bounds, listArray)
import Data.Char (isDigit)
import Data.List (group)
import Debug.Trace (trace)
import System.Environment (getArgs)

type Coord = (Int, Int)

data Robot = Robot {pos :: Coord, vel :: Coord} deriving (Eq, Show)

data Quadrant = TopLeft | TopRight | BottomLeft | BottomRight | None deriving (Eq, Show)

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

visualizeRobots :: Coord -> [Robot] -> Array Coord Char
visualizeRobots (width, height) robots = array ((0, 0), (width - 1, height - 1)) [((x, y), if (x, y) `elem` map pos robots then '*' else '.') | x <- [0 .. width - 1], y <- [0 .. height - 1]]

main = do
  args <- getArgs
  raw <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let size = if head args == "input.txt" then (101, 103) else (11, 7) :: Coord
  let robots = map parseRobot (lines raw)
  -- let robots_moved = map visualizeRobots $ take 10 $ iterate (step size) robots
  -- print $ robots
  putStrLn $ prettyPrint $ visualizeRobots size robots

-- moves the robot according to its own velocity
-- will wrap around the grid if border is reached!
moveRobot :: Coord -> Robot -> Robot
moveRobot (width, height) (Robot (px, py) (vx, vy)) = Robot ((px + vx) `mod` width, (py + vy) `mod` height) (vx, vy)

step :: Coord -> [Robot] -> [Robot]
step size = map (moveRobot size)

prettyPrint :: Array (Int, Int) Char -> String
prettyPrint arr =
  let ((rowStart, colStart), (rowEnd, colEnd)) = bounds arr
      rows = [[arr ! (r, c) | c <- [colStart .. colEnd]] | r <- [rowStart .. rowEnd]]
   in unlines $ map (unwords . map (: [])) rows

import Data.Array (Array, array, range)
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

main = do
  args <- getArgs
  raw <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let size = if head args == "input.txt" then (101, 103) else (11, 7) :: Coord
  let robots = map parseRobot (lines raw)
  let robots_moved = (!! 100) $ iterate (step size) robots
  let robots_quadrants = map (quadrant size) robots_moved
  let robots_TopLeft = length $ filter (== TopLeft) robots_quadrants
  let robots_TopRight = length $ filter (== TopRight) robots_quadrants
  let robots_BottomLeft = length $ filter (== BottomLeft) robots_quadrants
  let robots_BottomRight = length $ filter (== BottomRight) robots_quadrants
  print $ robots_TopLeft * robots_TopRight * robots_BottomLeft * robots_BottomRight

-- moves the robot according to its own velocity
-- will wrap around the grid if border is reached!
moveRobot :: Coord -> Robot -> Robot
moveRobot (width, height) (Robot (px, py) (vx, vy)) = Robot ((px + vx) `mod` width, (py + vy) `mod` height) (vx, vy)

step :: Coord -> [Robot] -> [Robot]
step size = map (moveRobot size)

quadrant :: Coord -> Robot -> Quadrant
quadrant (width, height) (Robot (px, py) vel)
  | px < cx && py < cy = TopLeft
  | px > cx && py < cy = TopRight
  | px < cx && py > cy = BottomLeft
  | py > cx && py > cy = BottomRight
  | otherwise = None
  where
    -- calculate cutoffs
    cx = width `div` 2
    cy = height `div` 2

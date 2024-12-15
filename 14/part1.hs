import Data.Array (Array, array, range)
import Data.Char (isDigit)
import Debug.Trace (trace)
import System.Environment (getArgs)

type Coord = (Int, Int)

data Robot = Robot {pos :: Coord, vel :: Coord} deriving (Eq, Show)

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
  print $ take 5 $ iterate (moveRobot size) (head robots)

-- moves the robot according to its own velocity
-- will wrap around the grid if border is reached!
moveRobot :: Coord -> Robot -> Robot
moveRobot (width, height) (Robot (px, py) (vx, vy)) = Robot ((px + vx) `mod` width, (py + vy) `mod` height) (vx, vy)

step :: [Robot] -> [Robot]
step rs = []

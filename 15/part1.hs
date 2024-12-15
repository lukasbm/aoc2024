import Data.Array
import System.Environment (getArgs)
import Prelude hiding (Left, Right)

type Coord = (Int, Int)

data WarehouseCell = Free | Wall | Box | Robot deriving (Eq)

instance Show WarehouseCell where
  show Wall = "#"
  show Box = "O"
  show Robot = "@"
  show Free = "."

type Warehouse = Array Coord WarehouseCell

-- starting with coords from top left
parseWarehouse :: [[Char]] -> Warehouse
parseWarehouse c =
  let height = length c
      width = length $ head c
   in listArray ((0, 0), (width - 1, height - 1)) $ map parseWarehouseCell (concat c)

parseWarehouseCell :: Char -> WarehouseCell
parseWarehouseCell '@' = Robot
parseWarehouseCell '#' = Wall
parseWarehouseCell 'O' = Box
parseWarehouseCell '.' = Free
parseWarehouseCell c = error $ "invalid cell: " <> show c

prettyPrint :: Array (Int, Int) Char -> String
prettyPrint arr =
  let ((rowStart, colStart), (rowEnd, colEnd)) = bounds arr
      rows = [[arr ! (r, c) | c <- [colStart .. colEnd]] | r <- [rowStart .. rowEnd]]
   in unlines $ map (unwords . map (: [])) rows

data Move = Left | Right | Up | Down deriving (Eq)

instance Show Move where
  show Left = "<"
  show Right = ">"
  show Up = "^"
  show Down = "v"

parseStep :: Char -> Move
parseStep '<' = Left
parseStep 'v' = Down
parseStep '>' = Right
parseStep '^' = Up

-- Function to get indices that match a condition
-- explanation:
-- assocs: Extracts all (index, value) pairs from the array.
-- then we apply the predicate
filterIndices :: (Ix i) => (e -> Bool) -> Array i e -> [i]
filterIndices predicate arr = [idx | (idx, val) <- assocs arr, predicate val]

main = do
  args <- getArgs
  raw <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let (warehouse_raw, moves_raw) = break (== "") $ lines raw
  let warehouse = parseWarehouse warehouse_raw
  let moves = map parseStep $ concat $ tail moves_raw
  print warehouse
  print moves
  let warehouse_cleaned = foldl step warehouse moves
  print warehouse_cleaned

coordinates :: Warehouse -> [Int]
coordinates g = map (\(x, y) -> x + 100 * y) $ filterIndices (== Box) g

-- pushBoxes :: Warehouse -> Coord -> Move -> Warehouse
-- pushBoxes w pos m =

-- FIXME: finding the robot every time might be a bit slow.
-- Use state monad to keep track across calls? or just add pos param and make the result (Coord, Warehouse)
step :: Warehouse -> Move -> Warehouse
step warehouse move
  | nextPos move robot_pos == Wall = warehouse
  | nextPos move robot_pos == Free = moveRobot warehouse
  | nextPos move robot_pos == Box && canPush = moveRobot $ pushBoxes warehouse (nextPos move robot_pos) 
  | otherwise = warehouse
  where
    robot_pos = head $ filterIndices (== Robot) w
    canPush == last (directionCells m robot_pos) == Free

    directionCells :: Move -> Coord -> [WarehouseCell]
    directionCells dir pos = if w ! nextPos dir pos == Wall then [w ! pos] else (w ! pos) : directionCells dir (nextPos dir pos)

    pushBoxes :: Warehouse -> Move -> Coord -> Warehouse
    pushBoxes w m pos
      | w ! pos == Free = w
      | (pushBoxes w  m  new_pos) // [(pos, Free), (new_pos, Box)]
      where
        new_pos = nextPos m pos

    -- assumes the next pos is free!!
    moveRobot :: Warehouse -> Warehouse
    moveRobot w = w // [(robot_pos, Free), (nextPos m robot_pos, Robot)]


nextPos :: Move -> Coord -> Coord
nextPos Up (x, y) = (x, y - 1)
nextPos Down (x, y) = (x, y + 1)
nextPos Left (x, y) = (x - 1, y)
nextPos Right (x, y) = (x + 1, y)

import Data.Array
import Debug.Trace (trace)
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
   in listArray ((0, 0), (height - 1, width - 1)) $ map parseWarehouseCell (concat c)

parseWarehouseCell :: Char -> WarehouseCell
parseWarehouseCell '@' = Robot
parseWarehouseCell '#' = Wall
parseWarehouseCell 'O' = Box
parseWarehouseCell '.' = Free
parseWarehouseCell c = error $ "invalid cell: " <> show c

prettyPrint :: Array (Int, Int) WarehouseCell -> String
prettyPrint arr =
  let ((rowStart, colStart), (rowEnd, colEnd)) = bounds arr
      rows = [[arr ! (r, c) | c <- [colStart .. colEnd]] | r <- [rowStart .. rowEnd]]
   in unlines $ map (concatMap ((: []) . head . show)) rows

data Move = Left | Right | Up | Down deriving (Eq)

instance Show Move where
  show Left = "<"
  show Right = ">"
  show Up = "^"
  show Down = "v"

parseStep :: Char -> Move
parseStep '<' = Left
parseStep '>' = Right
parseStep '^' = Up
parseStep 'v' = Down

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
  let robot_pos = head $ filterIndices (== Robot) warehouse

  print moves
  print robot_pos
  -- print "warehouse initial"
  -- putStrLn $ prettyPrint $ warehouse

  let warehouse_moved = snd $ foldl (\(robot_pos, w) move -> trace ("doing a move: " <> show move <> " on pos " <> show robot_pos) $ step w move robot_pos) (robot_pos, warehouse) moves
  putStrLn $ prettyPrint $ warehouse_moved
  print $ sum $ coordinates warehouse_moved

coordinates :: Warehouse -> [Int]
coordinates g = map (\(y, x) -> x + 100 * y) $ filterIndices (== Box) g

step :: Warehouse -> Move -> Coord -> (Coord, Warehouse)
step warehouse move robot_pos
  | warehouse ! next_robot_pos == Wall = (robot_pos, warehouse)
  | warehouse ! next_robot_pos == Free = (next_robot_pos, moveRobot warehouse)
  | warehouse ! next_robot_pos == Box && canPush = (next_robot_pos, moveRobot $ pushBoxes warehouse move next_robot_pos)
  | otherwise = (robot_pos, warehouse)
  where
    next_robot_pos = nextPos move robot_pos
    canPush = last (directionCells move next_robot_pos) == Free

    -- the cells until next free or wall! (first free is still included)
    directionCells :: Move -> Coord -> [WarehouseCell]
    directionCells dir pos = case warehouse ! pos of
      Wall -> []
      Free -> [Free]
      Box -> Box : directionCells dir (nextPos dir pos)

    pushBoxes :: Warehouse -> Move -> Coord -> Warehouse
    pushBoxes w m pos
      | w ! pos == Free = w
      | otherwise = pushBoxes w m new_pos // [(pos, Free), (new_pos, Box)]
      where
        new_pos = nextPos m pos

    -- assumes the next pos is free!!
    moveRobot :: Warehouse -> Warehouse
    moveRobot w = w // [(robot_pos, Free), (nextPos move robot_pos, Robot)]

nextPos :: Move -> Coord -> Coord
nextPos Up (y, x) = (y - 1, x)
nextPos Down (y, x) = (y + 1, x)
nextPos Left (y, x) = (y, x - 1)
nextPos Right (y, x) = (y, x + 1)

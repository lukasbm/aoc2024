import Data.Array
import Data.List (sortBy)
import Debug.Trace (trace)
import System.Environment (getArgs)
import Prelude hiding (Left, Right)

type Coord = (Int, Int)

data WarehouseCell = Free | Wall | BoxLeft | BoxRight | Robot deriving (Eq)

instance Show WarehouseCell where
  show Wall = "#"
  show BoxLeft = "["
  show BoxRight = "]"
  show Robot = "@"
  show Free = "."

type Warehouse = Array Coord WarehouseCell

-- starting with coords from top left
parseWarehouse :: [[Char]] -> Warehouse
parseWarehouse c =
  let height = length c
      width = 2 * length (head c)
   in listArray ((0, 0), (height - 1, width - 1)) $ concatMap parseWarehouseCell (concat c)

parseWarehouseCell :: Char -> [WarehouseCell]
parseWarehouseCell '@' = [Robot, Free]
parseWarehouseCell '#' = [Wall, Wall]
parseWarehouseCell 'O' = [BoxLeft, BoxRight]
parseWarehouseCell '.' = [Free, Free]
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
  print "warehouse initial"
  putStrLn $ prettyPrint $ warehouse

  let warehouse_moved = snd $ foldl (\(robot_pos, w) move -> step w move robot_pos) (robot_pos, warehouse) moves

  print $ "warehouse after"
  putStrLn $ prettyPrint $ warehouse_moved
  -- FIXME: wrong result for example1, but correct on example 3
  print $ sum $ coordinates warehouse_moved

coordinates :: Warehouse -> [Int]
coordinates g = map coordinate $ filterIndices (== BoxLeft) g

coordinate :: Coord -> Int
coordinate (y, x) = x + 100 * y

step :: Warehouse -> Move -> Coord -> (Coord, Warehouse)
step warehouse move robot_pos
  | warehouse ! next_robot_pos == Wall = (robot_pos, warehouse)
  | warehouse ! next_robot_pos == Free = (next_robot_pos, moveRobot warehouse)
  | move `elem` [Up, Down] && canMoveVertical = (next_robot_pos, moveRobot $ pushBoxes moveVertical)
  | move `elem` [Left, Right] && canMoveHorizontal = (next_robot_pos, moveRobot $ pushBoxes moveHorizontal)
  | otherwise = (robot_pos, warehouse)
  where
    next_robot_pos = nextPos move robot_pos

    -- always start it on next_robot_pos!
    horizontalMovement :: Coord -> [Coord]
    horizontalMovement pos = case warehouse ! pos of
      Wall -> []
      Free -> []
      BoxLeft -> pos : horizontalMovement (nextPos move pos)
      BoxRight -> pos : horizontalMovement (nextPos move pos)

    -- always start it on next_robot_pos!
    verticalMovement :: Coord -> [Coord]
    verticalMovement pos =
      let ml = nextPos Left pos
          mr = nextPos Right pos
       in case warehouse ! pos of
            Wall -> []
            Free -> []
            BoxLeft -> [pos, mr] ++ verticalMovement (nextPos move mr) ++ verticalMovement (nextPos move pos)
            BoxRight -> [pos, ml] ++ verticalMovement (nextPos move ml) ++ verticalMovement (nextPos move pos)

    moveVertical :: [Coord]
    moveVertical = verticalMovement next_robot_pos

    -- checks if in move direction there is no wall directly above any of the boxes i want to move vertically
    canMoveVertical :: Bool
    canMoveVertical = all (\pos -> (warehouse ! nextPos move pos) `elem` [Free, BoxLeft, BoxRight]) moveVertical

    moveHorizontal :: [Coord]
    moveHorizontal = horizontalMovement next_robot_pos

    -- checks if in move direction there is no wall directly above any of the boxes i want to move horizontally
    canMoveHorizontal :: Bool
    canMoveHorizontal = all (\pos -> (warehouse ! nextPos move pos) `elem` [Free, BoxLeft, BoxRight]) moveHorizontal

    -- first set it all to free, then replace with the new coords
    -- assumes its free!
    -- to avoid issues while writing, we need to sort the coords:
    -- best, sort by coordinate function
    pushBoxes :: [Coord] -> Warehouse
    pushBoxes cs =
      let values = [warehouse ! c | c <- cs]
          new_cs = map (nextPos move) cs
          warehouse_clean = warehouse // ((,Free) <$> cs)
          warehouse_filled = warehouse_clean // zip new_cs values
       in warehouse_filled

    -- update robot position
    -- assumes the next pos is free!!
    moveRobot :: Warehouse -> Warehouse
    moveRobot w = w // [(robot_pos, Free), (next_robot_pos, Robot)]

nextPos :: Move -> Coord -> Coord
nextPos Up (y, x) = (y - 1, x)
nextPos Down (y, x) = (y + 1, x)
nextPos Left (y, x) = (y, x - 1)
nextPos Right (y, x) = (y, x + 1)

shiftArray :: Array Coord a -> Move -> a -> Array Coord a
shiftArray arr direction filler = array bnds updatedElems
  where
    bnds@((xmin, ymin), (xmax, ymax)) = bounds arr
    updatedElems = [((x, y), newElem (x, y)) | x <- [xmin .. xmax], y <- [ymin .. ymax]]
    newElem (x, y) = case direction of
      Left -> if x == xmax then filler else arr ! (x + 1, y)
      Right -> if x == xmin then filler else arr ! (x - 1, y)
      Up -> if y == ymax then filler else arr ! (x, y + 1)
      Down -> if y == ymin then filler else arr ! (x, y - 1)

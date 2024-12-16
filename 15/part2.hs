import Data.Array
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

  let steps = 11
  let warehouse_moved = snd $ foldl (\(robot_pos, w) move -> trace ("doing a move: " <> show move <> " on pos " <> show robot_pos) $ step w move robot_pos) (robot_pos, warehouse) (take steps moves)

  print "hi"

  print $ "warehouse after " <> show steps <> " steps"
  putStrLn $ prettyPrint $ warehouse_moved

-- print $ sum $ coordinates warehouse_moved

coordinates :: Warehouse -> [Int]
coordinates g = map (\(y, x) -> x + 100 * y) $ filterIndices (== BoxLeft) g

step :: Warehouse -> Move -> Coord -> (Coord, Warehouse)
step warehouse move robot_pos
  | warehouse ! next_robot_pos == Wall = (robot_pos, warehouse)
  | warehouse ! next_robot_pos == Free = (next_robot_pos, moveRobot warehouse)
  | move `elem` [Up, Down] && canMoveVertical = trace ("moving the vertical boxes: " <> show moveVertical <> " in direction: " <> show move) (next_robot_pos, moveRobot $ pushBoxes moveVertical)
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
       in trace ("veritcal movemvent on: " <> show pos) $ case warehouse ! pos of
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
    -- FIXME: idk if this works
    pushBoxes :: [Coord] -> Warehouse
    pushBoxes = foldr (flip pushCell) warehouse

    -- assume we are allowed to do it!
    pushCell :: Warehouse -> Coord -> Warehouse
    pushCell w pos =
      let new_pos = nextPos move pos
          el = w ! pos
       in w // [(pos, Free), (new_pos, el)]

    -- assumes the next pos is free!!
    moveRobot :: Warehouse -> Warehouse
    moveRobot w = w // [(robot_pos, Free), (nextPos move robot_pos, Robot)]

nextPos :: Move -> Coord -> Coord
nextPos Up (y, x) = (y - 1, x)
nextPos Down (y, x) = (y + 1, x)
nextPos Left (y, x) = (y, x - 1)
nextPos Right (y, x) = (y, x + 1)

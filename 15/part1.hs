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

main = do
  args <- getArgs
  raw <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let (warehouse_raw, moves_raw) = break (== "") $ lines raw
  let warehouse = parseWarehouse warehouse_raw
  let moves = map parseStep $ concat $ tail moves_raw
  print warehouse
  print moves

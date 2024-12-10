import Data.Array
import Data.List (find, intercalate)
import Data.Maybe (isJust, isNothing)
import System.Environment (getArgs)

-- since it has Ord i can just to guard > neighbor to check if i am free to move
data Object = Free | GuardLeft | GuardRight | GuardUp | GuardDown | Obstacle | Patrolled deriving (Eq, Enum, Ord)

type Coord = (Int, Int)

instance Show Object where
  show GuardLeft = "<"
  show GuardRight = ">"
  show GuardUp = "^"
  show GuardDown = "v"
  show Free = "."
  show Obstacle = "#"
  show Patrolled = "X"

parseObject :: Char -> Object
parseObject '.' = Free
parseObject '#' = Obstacle
parseObject 'v' = GuardDown
parseObject '>' = GuardRight
parseObject '<' = GuardLeft
parseObject '^' = GuardUp
parseObject 'X' = Patrolled
parseObject _ = error "invalid object"

isGuard :: Object -> Bool
isGuard GuardDown = True
isGuard GuardUp = True
isGuard GuardLeft = True
isGuard GuardRight = True
isGuard _ = False

toArray :: [[a]] -> Array Coord a
toArray xss =
  let rows = length xss
      cols = if null xss then 0 else length (head xss)
      bounds = ((0, 0), (rows - 1, cols - 1))
      elems = concat xss
   in array bounds (zip (range bounds) elems)

main = do
  args <- getArgs
  raw_text <- if length args == 1 then readFile $ head args else error "Usage: ./program <file>"
  let grid_raw = toArray $ (map . map) parseObject $ lines raw_text
  print $ grid_raw ! (0, 1)

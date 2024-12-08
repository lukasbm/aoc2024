import Data.List (find, intercalate)
import Data.Maybe (isJust, isNothing)

data Object = GuardLeft | GuardRight | GuardUp | GuardDown | Free | Obstacle | Patrolled deriving (Eq)

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

main = do
  raw_text <- getContents
  let grid_raw = (map . map) parseObject $ lines raw_text :: [[Object]]
  print grid_raw

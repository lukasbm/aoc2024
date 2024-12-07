data Object = GuardLeft | GuardRight | GuardUp | GuardDown | Free | Obstacle | Patrolled deriving (Show, Eq)

type Grid = [[Object]]

parseObject :: Char -> Object
parseObject '.' = Free
parseObject '#' = Obstacle
parseObject 'v' = GuardDown
parseObject '>' = GuardRight
parseObject '<' = GuardLeft
parseObject '^' = GuardUp
parseObject 'X' = Patrolled
parseObject _ = error "invalid object"

main = do
  raw_text <- getContents
  let grid = (map . map) parseObject $ lines raw_text
  print $ grid

leavingArea :: Grid -> Bool
leavingArea g = False

step :: Grid -> Grid
step g = g

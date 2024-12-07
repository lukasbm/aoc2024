{-# LANGUAGE FlexibleInstances #-}

import Data.List (intercalate)

data Object = GuardLeft | GuardRight | GuardUp | GuardDown | Free | Obstacle | Patrolled deriving (Eq)

instance Show Object where
  show GuardLeft = "<"
  show GuardRight = ">"
  show GuardUp = "^"
  show GuardDown = "v"
  show Free = "."
  show Obstacle = "#"
  show Patrolled = "X"

newtype Grid = Grid [[Object]] deriving (Eq)

instance Show Grid where
  -- show (Grid g) = intercalate "\n" $ map (concatMap show) g
  show (Grid rows) = unlines (map (concatMap show) rows)

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
  let grid = Grid $ (map . map) parseObject $ lines raw_text
  print grid

walkUntilLeave :: Grid -> Int
walkUntilLeave g = 0

leavingArea :: Grid -> Bool
leavingArea g = False

step :: Grid -> Grid
step g = g

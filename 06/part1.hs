{-# LANGUAGE FlexibleInstances #-}

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

newtype Grid = Grid [[Object]] deriving (Eq)

instance Show Grid where
  -- show (Grid g) = intercalate "\n" $ map (concatMap show) g
  show (Grid rows) = unlines (map (concatMap show) rows)

-- instance Foldable Grid where
--   foldMap f (Grid rows) = foldMap (foldMap f) rows

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
  let grid = Grid $ (map . map) parseObject $ lines raw_text
  print grid

walkUntilLeave :: Grid -> Int
walkUntilLeave g = if leavingArea g then 1 else 1 + walkUntilLeave (step g)

-- assuming there always has to be a guard:
-- when we cut of the border (top, left, right and bottom rows/cols) and don't find one --> guard was at the border
leavingArea :: Grid -> Bool
leavingArea (Grid g) =
  let borders = head g ++ last g ++ concatMap (\row -> [head row, last row]) (init (tail g))
   in isJust $ find isGuard borders


-- TODO: the hardest part?
step :: Grid -> Grid
step g = g

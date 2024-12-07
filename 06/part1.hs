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

withIndices :: Grid -> [(Int, Int, Object)]
withIndices (Grid arr) = [(row, col, arr !! row !! col) | row <- [0 .. length arr - 1], col <- [0 .. length (head arr) - 1]]

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

-- returns the neigh
neighbors :: [[(Int, Int, a)]] -> (Int, Int, a) -> a -> [a]
neighbors grid needle@(row, col, val) filler = [val]
  where
    safeIndex y x = if x < 0 || y < 0 || x >= length grid || y >= length (grid !! 0) then '.' else grid !! y !! x

updateState :: [[(Int, Int, Object)]] -> (Int, Int, Object) -> Grid
updateState 

-- TODO: the hardest part?
step :: Grid -> Grid
step grid =
  let indexedGrid = withIndices grid
    in case find (\(_,_,v) -> isGuard v) indexedGrid of
      Just guard =  updateState 
      Nothing = error "fixme"

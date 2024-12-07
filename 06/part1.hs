{-# LANGUAGE FlexibleInstances #-}

import Data.List (find, intercalate)
import Data.Maybe (isJust, isNothing)

cols :: [[a]] -> Int
cols xss = length (head xss)

b2i :: Bool -> Int
b2i False = 0
b2i True = 1

rows :: [[a]] -> Int
rows = length

chunked :: Int -> [a] -> [[a]]
chunked _ [] = []
chunked n xs = take n xs : chunked n (drop n xs)

-- | 'slidingWindow2' applies a function to every subgrid of a given size within a grid.
-- The first parameter is the size of the subgrid (number of rows and columns).
-- The second parameter is the function to apply to each subgrid.
-- The third parameter is the grid to process.
slidingWindow2 :: (Int, Int) -> ([[a]] -> b) -> [[a]] -> [b]
slidingWindow2 size@(nrow, ncol) func xss =
  [func (take nrow $ map (take ncol . drop j) (drop i xss)) | i <- [0 .. (rows xss - nrow)], j <- [0 .. (cols xss - ncol)]]

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

-- looks at the neighborhood (3x3 grid) and updates the center accordingly
evaluateNeighborhood :: [[Object]] -> Object
evaluateNeighborhood idk = head $ head idk

step :: Grid -> Grid
step (Grid grid) =
  let updatedInner = chunked (cols grid - 2) $ slidingWindow2 (3, 3) evaluateNeighborhood grid
   in Grid $ [head grid] ++ zipWith (\innerRow fullRow -> [head fullRow] ++ innerRow ++ [last fullRow]) updatedInner grid ++ [last grid]

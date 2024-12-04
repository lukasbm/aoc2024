import Data.List (find)
import Data.Maybe (fromMaybe)
import Prelude hiding (Left, Right)

type Elem = (Int, Int, Char)

data Direction = UpLeft | Up | UpRight | Right | DownRight | Down | DownLeft | Left deriving (Show, Eq)

b2i :: Bool -> Int
b2i False = 0
b2i True = 1

value :: Elem -> Char
value (_, _, v) = v

iterateWithIndices :: [[Char]] -> [Elem]
iterateWithIndices arr = [(row, col, (arr !! row) !! col) | row <- [0 .. length arr - 1], col <- [0 .. length (head arr) - 1]]

main = do
  txt <- readFile "test_part1.txt"
  let grid_raw = lines txt :: [[Char]]
  let grid = iterateWithIndices grid_raw :: [Elem]
  -- print $ grid
  -- print $ walk grid (grid !! 4) DownRight
  -- print $ walk grid (grid !! 4) (step DownRight) []
  print $ length $ filter (\(y, x, v) -> v == 'X') grid
  print $ sum $ map (b2i . walkAllDir grid) $ filter (\(y, x, v) -> v == 'X') grid

step :: Direction -> [Elem] -> Elem -> Elem
step UpLeft g (y, x, v) = fromMaybe (y - 1, x, '.') $ find (\(yn, xn, vn) -> yn == y - 1 && xn == x - 1) g
step Up g (y, x, v) = fromMaybe (y - 1, x, '.') $ find (\(yn, xn, vn) -> yn == y - 1 && xn == x) g
step UpRight g (y, x, v) = fromMaybe (y - 1, x + 1, '.') $ find (\(yn, xn, vn) -> yn == y - 1 && xn == x + 1) g
step Right g (y, x, v) = fromMaybe (y, x + 1, '.') $ find (\(yn, xn, vn) -> yn == y && xn == x + 1) g
step DownRight g (y, x, v) = fromMaybe (y + 1, x + 1, '.') $ find (\(yn, xn, vn) -> yn == y + 1 && xn == x + 1) g
step Down g (y, x, v) = fromMaybe (y + 1, x, '.') $ find (\(yn, xn, vn) -> yn == y + 1 && xn == x) g
step DownLeft g (y, x, v) = fromMaybe (y + 1, x - 1, '.') $ find (\(yn, xn, vn) -> yn == y + 1 && xn == x - 1) g
step Left g (y, x, v) = fromMaybe (y, x - 1, '.') $ find (\(yn, xn, vn) -> yn == y && xn == x - 1) g

-- checks if i can walk "XMAS" from the current direction
walk :: [Elem] -> Elem -> ([Elem] -> Elem -> Elem) -> [Char] -> Bool
walk g e@(y, x, v) step ['X', 'M', 'A', 'S'] = True
walk g e@(y, x, v) step ['X', 'M', 'A'] = walk g (step g e) step ['X', 'M', 'A', v]
walk g e@(y, x, v) step ['X', 'M'] = walk g (step g e) step ['X', 'M', v]
walk g e@(y, x, v) step ['X'] = walk g (step g e) step ['X', v]
walk g e@(y, x, v) step [] = walk g (step g e) step [v]
walk g e@(y, x, v) step _ = False

walkAllDir :: [Elem] -> Elem -> Bool
walkAllDir g e =
  or
    [ walk g e (step UpLeft) [],
      walk g e (step Up) [],
      walk g e (step UpRight) [],
      walk g e (step Right) [],
      walk g e (step DownRight) [],
      walk g e (step Down) [],
      walk g e (step DownLeft) [],
      walk g e (step Left) []
    ]

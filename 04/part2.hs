import Data.List (find, nub, transpose)
import Data.Maybe (fromMaybe)
import Prelude hiding (Left, Right)

b2i :: Bool -> Int
b2i False = 0
b2i True = 1

cols :: [[a]] -> Int
cols xss = length (head xss)

rows :: [[a]] -> Int
rows = length

-- | 'slidingWindow2' applies a function to every subgrid of a given size within a grid.
-- The first parameter is the size of the subgrid (number of rows and columns).
-- The second parameter is the function to apply to each subgrid.
-- The third parameter is the grid to process.
slidingWindow2 :: (Int, Int) -> ([[a]] -> b) -> [[a]] -> [b]
slidingWindow2 size@(nrow, ncol) func xss =
  [func (take nrow $ map (take ncol . drop j) (drop i xss)) | i <- [0 .. (rows xss - nrow)], j <- [0 .. (cols xss - ncol)]]

main :: IO ()
main = readFile "input.txt" >>= print . sum . map b2i . slidingWindow2 (3, 3) xmas . lines

xmas :: [[Char]] -> Bool
-- xmas [[tl, t, tr], [ml, m, mr], [bl, b, br]]
xmas [['M', t, 'M'], [ml, 'A', mr], ['S', b, 'S']] = True -- Ms on top
xmas [['M', t, 'S'], [ml, 'A', mr], ['M', b, 'S']] = True -- Ms on left
xmas [['S', t, 'M'], [ml, 'A', mr], ['S', b, 'M']] = True -- Ms on right
xmas [['S', t, 'S'], [ml, 'A', mr], ['M', b, 'M']] = True -- Ms on bot
xmas _ = False

-- main = do  -- verifying xmas fn!
--   print $ xmas ["S.S",".A.","M.M"]
--   print $ xmas ["S.S",".A.","M.M"]
--   print $ xmas ["S.S",".A.","M.M"]
--   print $ xmas ["S.S",".A.","M.M"]
--   print $ xmas ["M.S",".A.","M.S"]
--   print $ xmas ["S.M",".AS","S.M"]
--   print $ xmas ["MSM","MAA","SMS"]
--   print $ xmas ["SMS","AA.","MSM"]
--   print $ xmas ["M.S",".A.","M.S"]

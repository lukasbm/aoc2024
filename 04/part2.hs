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

-- 2 Dimension sliding window
slidingWindow2 :: (Int, Int) -> ([[a]] -> b) -> [[a]] -> [b]
slidingWindow2 size@(nrow, ncol) func xss =
  [func (take nrow $ map (take ncol) (drop i xss)) | i <- [0 .. (rows xss - nrow)], j <- [0 .. (cols xss - ncol)]]

main :: IO ()
main = readFile "test_part2.txt" >>= print . sum . map b2i . slidingWindow2 (3, 3) xmas . lines

-- main = readFile "test_part2.txt" >>= print . slidingWindow2 (3, 3) id . lines

xmas :: [[Char]] -> Bool
-- xmas [[tl, t, tr], [ml, m, mr], [bl, b, br]]
xmas [['M', t, 'M'], [ml, 'A', mr], ['S', b, 'S']] = True -- Ms on top
xmas [['M', t, 'S'], [ml, 'A', mr], ['M', b, 'S']] = True -- Ms on left
xmas [['S', t, 'M'], [ml, 'A', mr], ['S', b, 'M']] = True -- Ms on right
xmas [['S', t, 'S'], [ml, 'A', mr], ['M', b, 'M']] = True -- Ms on bot
xmas _ = False

-- for test data: should only be 64 boolean values (64 applications of xmas func)!

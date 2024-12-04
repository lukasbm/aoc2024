import Data.List (find, nub, transpose)
import Data.Maybe (fromMaybe)
import Prelude hiding (Left, Right)

cols :: [[a]] -> Int
cols xss = length (head xss)

rows :: [[a]] -> Int
rows = length

slidingWindow :: (Int, Int) -> ([[a]] -> b) -> [[a]] -> [b]
slidingWindow (nrow, ncol) func xss
  | rows xss < nrow = []
  | cols xss < ncol = []
slidingWindow size@(nrow, ncol) func xss =
  let window = take nrow $ map (take ncol) xss
   in [func window] ++ slidingWindow size func (drop 1 xss) ++ slidingWindow size func (map (drop 1) xss)

main :: IO ()
main = readFile "input.txt" >>= print . slidingWindow (3, 3) xmas . lines

xmas :: [[Char]] -> Bool
-- xmas [[tl, t, tr], [ml, m, mr], [bl, b, br]]
xmas [['M', t, 'M'], [ml, 'A', mr], ['S', b, 'S']] = True -- Ms on top
xmas [['M', t, 'S'], [ml, 'A', mr], ['M', b, 'S']] = True -- Ms on left
xmas [['S', t, 'M'], [ml, 'A', mr], ['S', b, 'M']] = True -- Ms on right
xmas [['S', t, 'S'], [ml, 'A', mr], ['M', b, 'M']] = True -- Ms on bot
xmas _ = False

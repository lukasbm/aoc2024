import Data.List (find, nub, transpose)
import Data.Maybe (fromMaybe)
import Prelude hiding (Left, Right)

type Grid = [[Char]]

cols :: [[a]] -> Int
cols xss = length (head xss)

rows :: [[a]] -> Int
rows = length

slidingWindow :: [[a]] -> (Int, Int) -> ([[a]] -> b) -> [b]
slidingWindow xss (nrow, ncol) func
  | rows xss < nrow = 0
  | cols xss < ncol = 0
slidingWindow xss size@(nrow, ncol) func =
  [func (take nrow $ map (take ncol) xss)] ++ slidingWindow (drop 1 xss) size func ++ slidingWindow (map (drop 1) xss) size func

main :: IO ()
main = readFile "input.txt" >>= print . sum . map count . nub . allText . lines

matches2d_2 :: (Eq a) => [[a]] -> [[a]] -> Int
matches2d_2 pattern xss | shorter xss pattern = 0 -- not enough rows
matches2d_2 pattern (xs : _) | shorter xs (head pattern) = 0 -- not enough columns
matches2d_2 pattern xss =
  sum [1 | all and (zipWith (zipWith (==)) pattern xss)] -- this match
    + matches2d_2 pattern (drop 1 xss) -- next row
    + matches2d_2 pattern (map (drop 1) xss) -- next col

shorter a b = length a < length b

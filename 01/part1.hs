import Data.List (sort)
import System.Environment (getArgs)

absdiff :: Int -> Int -> Int
absdiff a b = abs (a - b)

main = do
  args <- getArgs
  raw <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let raw_tuples = map (break (== ' ')) $ lines raw
  let left = map (read . fst) raw_tuples :: [Int]
  let right = map (read . snd) raw_tuples :: [Int]
  let differences = zipWith absdiff (sort left) (sort right)
  print $ sum differences

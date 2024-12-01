import Data.List (sort)

absdiff :: Int -> Int -> Int
absdiff a b = abs (a - b)

main = do
  raw <- readFile "input.txt"
  let raw_tuples = map (break (== ' ')) $ lines raw
  let left = map (read . fst) raw_tuples :: [Int]
  let right = map (read . snd) raw_tuples :: [Int]
  let differences = zipWith absdiff (sort left) (sort right)
  print $ sum differences

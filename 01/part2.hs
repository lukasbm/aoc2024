import Data.List (sort)

appearances :: [Int] -> Int -> Int
appearances xs x = length $ filter (== x) xs

main = do
  raw <- readFile "input.txt"
  let raw_tuples = map (break (== ' ')) $ lines raw
  let left = map (read . fst) raw_tuples :: [Int]
  let right = map (read . snd) raw_tuples :: [Int]
  print $ sum $ map (\x -> x * appearances right x) left

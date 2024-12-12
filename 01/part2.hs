import Data.List (sort)
import System.Environment (getArgs)

appearances :: [Int] -> Int -> Int
appearances xs x = length $ filter (== x) xs

main = do
  args <- getArgs
  raw <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let raw_tuples = map (break (== ' ')) $ lines raw
  let left = map (read . fst) raw_tuples :: [Int]
  let right = map (read . snd) raw_tuples :: [Int]
  print $ sum $ map (\x -> x * appearances right x) left

import Data.Bits (Bits (xor))
import System.Environment (getArgs)

step :: [Int] -> [Int]
step = foldl (\acc x -> acc ++ applyRules x) []
  where
    applyRules :: Int -> [Int]
    applyRules x = do
      let x1 = firstRule x
      if x1 /= [x]
        then x1
        else do
          let x2 = secondRule x
          if x2 /= [x]
            then x2
            else thirdRule x

    firstRule 0 = [1]
    firstRule x = [x]

    secondRule x
      | even nd = [read l, read r]
      | otherwise = [x]
      where
        nd = (length . show) x
        (l, r) = splitAt (nd `div` 2) (show x)

    thirdRule x = [x * 2024]

main = do
  args <- getArgs
  raw_text <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let stones = map read $ words raw_text :: [Int]
  print $ length $ last $ take 26 $ iterate step stones -- needs to be 26 to get after 25 iterations

import Data.Bits (Bits (xor))
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

step :: [Int] -> [Int]
step = foldl (\acc x -> acc ++ applyRules x) []
  where
    applyRules :: Int -> [Int]
    applyRules x = case firstRule x of
      Just x1 -> x1
      Nothing -> case secondRule x of
        Just x2 -> x2
        Nothing -> fromMaybe [x] (thirdRule x)

    firstRule 0 = Just [1]
    firstRule x = Nothing

    secondRule x
      | even nd = Just [read l, read r]
      | otherwise = Nothing
      where
        nd = (length . show) x
        (l, r) = splitAt (nd `div` 2) (show x)

    thirdRule x = Just [x * 2024]

main = do
  args <- getArgs
  raw_text <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let stones = map read $ words raw_text :: [Int]
  print $ (!! 25) $ iterate step stones -- needs to be 26 to get after 25 iterations

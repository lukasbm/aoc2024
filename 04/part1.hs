import Data.List (find, nub, transpose)
import Data.Maybe (fromMaybe)
import Prelude hiding (Left, Right)

type Grid = [[Char]]

diagonals :: Grid -> [String]
diagonals x =
  transpose (zipWith drop [0 ..] x)
    ++ transpose (zipWith drop [0 ..] (transpose x))
    ++ transpose (zipWith drop [0 ..] (map reverse x))
    ++ transpose (zipWith drop [0 ..] (transpose $ map reverse x))
    ++ transpose (zipWith drop [0 ..] (map reverse (transpose x)))

allText :: Grid -> [String]
allText x = diagonals x ++ map reverse (diagonals x) ++ x ++ map reverse x ++ transpose x ++ map reverse (transpose x)

count :: String -> Int
count "" = 0
count ('X' : 'M' : 'A' : 'S' : rest) = 1 + count rest
count (x : xs) = count xs

main :: IO ()
main = readFile "input.txt" >>= print . sum . map count . nub . allText . lines

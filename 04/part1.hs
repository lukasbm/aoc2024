import Data.List (find, nub, transpose)
import Data.Maybe (fromMaybe)
import Prelude hiding (Left, Right)

b2i :: Bool -> Int
b2i False = 0
b2i True = 1

type Grid = [[Char]]

-- TODO: need to join everything with backward self to account for backward

diagonals x =
  nub $
    transpose (zipWith drop [0 ..] x)
      ++ transpose (zipWith drop [0 ..] (transpose x))
      ++ transpose (zipWith drop [0 ..] (transpose $ map reverse x))
      ++ transpose (zipWith drop [0 ..] (map reverse x))

straights :: Grid -> String
straights = concat

verticals :: Grid -> String
verticals = concat . transpose

allText :: Grid -> [String]
allText x = nub $ diagonals x ++ reverse (diagonals x) ++ [straights x, reverse (straights x), verticals x, reverse (verticals x)]

count :: String -> Int
count "" = 0
count ('X' : 'M' : 'A' : 'S' : rest) = 1 + count rest
count (x : xs) = count xs

main :: IO ()
-- main = readFile "test_part1.txt" >>= print . sum . map count . allText . lines
main = readFile "test_part1.txt" >>= print . allText . lines

-- main =  print $ map count $ allText ["ABCD", "EFGH", "IJKL"]

-- map reverse vor dem concat!!!

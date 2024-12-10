{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Move filter" #-}

import Data.List (nub)
import System.Environment (getArgs)

type Frequency = Char

data Cell = Free | Antinode Frequency | Antenna Frequency deriving (Show, Eq)

type Coord = (Int, Int)

type Entry = (Coord, Cell)

type Grid = [Entry]

rows :: [[a]] -> Int
rows = length

cols :: [[a]] -> Int
cols x = length (head x)

main = do
  args <- getArgs
  raw_text <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let grid_raw = (map . map) (\x -> if x == '.' then Free else Antenna x) $ lines raw_text
  let grid = zip [(r, c) | r <- [0 .. (rows grid_raw - 1)], c <- [0 .. (cols grid_raw - 1)]] (concat grid_raw) :: Grid
  let unique_frequencies = nub $ map (\(Antenna x) -> x) $ filter (\x -> case x of Antenna f -> True; _ -> False) (concat grid_raw)
  let grid_size = (rows grid_raw, cols grid_raw)
  let pairs = concatMap (makePairs grid) unique_frequencies

  print $ evalPair grid_size (head pairs)
  print $ "hi"

-- print $ grid
-- print $ map (\(a, b) -> (fst a, fst b)) $ pairs grid '0'

-- print $ map (uncurry linearDiff) $ pairs grid '0'

-- eval pair while overshooting in one direction (a -> b -> antinode)
-- we want a + 2l = antinode and naturally a + l = b
-- pairs exist in both directions so (b -> a -> antinode) is evaluated in another call
evalPair :: Coord -> (Entry, Entry) -> Bool
evalPair grid_size ((a, _), (b, _)) =
  let l = linearSub a b
      antinode = linearAdd b l
   in linearAdd a (linearAdd l l) == b && inBounds grid_size antinode

-- given grid size, check if point is in bounds
inBounds :: Coord -> Coord -> Bool
inBounds grid@(gr, gc) antinode@(ar, ac) = ar >= 0 && ar < gr && ac >= 0 && ac < gc

-- b - a
linearSub :: Coord -> Coord -> Coord
linearSub (ar, ac) (br, bc) = (br - ar, bc - ac)

-- b + a
linearAdd :: Coord -> Coord -> Coord
linearAdd (ar, ac) (br, bc) = (br + ar, bc + ac)

-- gets all possible pairs of a frequency
-- bidirectional, so both (a,b) AND (b,a) will be included)
makePairs :: Grid -> Frequency -> [(Entry, Entry)]
makePairs grid freq =
  let coords = filter (\((r, c), v) -> v == Antenna freq) grid
   in [(a, b) | a <- coords, b <- coords, a /= b]

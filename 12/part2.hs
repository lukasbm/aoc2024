import Data.Array.IArray
import Data.Array.Unboxed
import Data.List (nub)
import Data.Set qualified as Set
import Debug.Trace (trace)
import System.Environment (getArgs)

type Coord = (Int, Int)

type Grid = UArray Coord Char

-- Convert 2D list to UArray
toUArray :: [[Char]] -> Grid
toUArray xs = array bounds elements
  where
    rows = length xs
    cols = if null xs then 0 else length (head xs)
    bounds = ((1, 1), (rows, cols)) -- Assuming 1-based indexing
    elements =
      [ ((i, j), xs !! (i - 1) !! (j - 1))
        | i <- [1 .. rows],
          j <- [1 .. cols]
      ]

-- neighbors in 4 directions if available
getNeighbors :: Grid -> Coord -> [Coord]
getNeighbors grid (i, j) =
  let ((minI, minJ), (maxI, maxJ)) = bounds grid
      potentialNeighbors = [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]
   in filter (\(i, j) -> i >= minI && i <= maxI && j >= minJ && j <= maxJ) potentialNeighbors

-- neighbors in 4 directions with no bounds check
getStupidNeighbors :: Grid -> Coord -> [Coord]
getStupidNeighbors grid (i, j) = [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]

main = do
  args <- getArgs
  raw_text <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let grid = toUArray $ lines raw_text :: Grid

  -- print $ grid
  -- print $ grid ! (2, 3)
  print $ floodFill grid [] (2, 1)
  print $ price grid $ floodFill grid [] (2, 1)

-- print $ solve grid

-- price = number of sides * area = number of corners * area
price :: Grid -> [Coord] -> Int
price g s =
  let area = length s
      sides = sum (map (corners g s) s)
   in area * sides

-- number of valid corners at on position in a region
-- assumes that x `elem` region
corners :: Grid -> [Coord] -> Coord -> Int
corners g region x =
  let neigh = getNeighbors g x
      -- diff is between 0 and 4 long
      diff = Set.toList $ Set.difference (Set.fromList neigh) (Set.fromList region)
      diff_pairs = (,head diff) <$> tail diff
   in  length $ filter inLine diff_pairs
  where
    inLine :: (Coord, Coord) -> Bool
    inLine ((a1, a2), (b1, b2)) = b1 - a1 == 0 || b2 - a2 == 0

solve :: Grid -> [Int]
solve g = map (price g) $ go $ Set.fromList (indices g)
  where
    go :: Set.Set Coord -> [[Coord]]
    go xs
      | Set.null xs = []
      | otherwise =
          let region = floodFill g [] (Set.elemAt 0 xs)
           in region : go (Set.difference xs $ Set.fromList region) -- go again but with region removed

-- each neighbor that is not of the same type adds 1 to the perimeter (for this cell)
-- out of bounds also counts as a different type
-- less than 4 coors on getNeighbors means out of bounds -> increase perimeter
-- result: list of coors and their perimeter
floodFill :: Grid -> [Coord] -> Coord -> [Coord]
floodFill grid visited pos =
  let sameNeighbors = filter (\x -> grid ! x == grid ! pos) $ getNeighbors grid pos
      unvisitedNeighbors = filter (`notElem` visited) sameNeighbors
   in pos : foldAlong (\acc visited neigh -> acc ++ floodFill grid visited neigh) (++) [] unvisitedNeighbors (unvisitedNeighbors ++ [pos] ++ visited)

-- old attempt ----- (pos, perimeter) concatMap (floodFill grid $ [pos] ++ unvisitedNeighbors ++ visited) unvisitedNeighbors

-- a = Coord
-- result accumulator: b = [(Coord, Int)]
-- helper c = [Coord] (visited)
-- the second function is essentially another helper to modify b
-- example usage:
-- print $ foldAlong (\acc helper x -> if x `elem` helper then acc else acc + x) const 0 [1 .. 5] [3]
-- add ups 1 to 5 except 3
-- very contrived example
foldAlong :: (b -> c -> a -> b) -> (c -> b -> c) -> b -> [a] -> c -> b
foldAlong _ _ acc [] _ = acc
foldAlong f h acc (x : xs) cs =
  let y = f acc cs x
   in foldAlong f h y xs (h cs y)

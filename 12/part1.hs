import Data.Array.IArray
import Data.Array.Unboxed
import Data.List (nub)
import Data.Set qualified as Set
import Debug.Trace (trace)
import System.Environment (getArgs)

type Coord = (Int, Int)

type Grid = UArray Coord Char

-- coords and perimeter values of each cell in a region
type Region = [(Coord, Int)]

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

main = do
  args <- getArgs
  raw_text <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let grid = toUArray $ lines raw_text :: Grid

  -- print $ grid
  -- print $ grid ! (2, 3)
  -- print $ floodFill grid [] (2, 1)
  -- print $ floodFill grid [] (2, 1)
  print $ sum $ solve grid

-- perimeter sum * area
price :: Region -> Int
price s = length s * sum (map snd s)

solve :: Grid -> [Int]
solve g = map price $ go $ Set.fromList (indices g)
  where
    go :: Set.Set Coord -> [Region]
    go xs
      | Set.null xs = []
      | otherwise =
          let region = floodFill g [] (Set.elemAt 0 xs)
           in region : go (Set.difference xs $ Set.fromList (map fst region)) -- go again but with region removed

-- each neighbor that is not of the same type adds 1 to the perimeter (for this cell)
-- out of bounds also counts as a different type
-- less than 4 coors on getNeighbors means out of bounds -> increase perimeter
-- result: list of coors and their perimeter
floodFill :: Grid -> [Coord] -> Coord -> Region
floodFill grid visited pos =
  let sameNeighbors = filter (\x -> grid ! x == grid ! pos) $ getNeighbors grid pos
      perimeter = 4 - length sameNeighbors
      unvisitedNeighbors = filter (`notElem` visited) sameNeighbors
   in (pos, perimeter) : foldAlong (\acc visited neigh -> acc ++ floodFill grid visited neigh) (\vs lastRes -> vs ++ map fst lastRes) [] unvisitedNeighbors (unvisitedNeighbors ++ [pos] ++ visited)

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

-- usage
-- test = foldAlong (\res vis x -> floodFill g) (\vis res -> diff vis $ map fst res) neighbors visited
--   where
--     g = null
--     visited = []

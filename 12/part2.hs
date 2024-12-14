import Data.Array.IArray
import Data.Array.Unboxed
import Data.List (nub)
import Data.Set qualified as Set
import Debug.Trace (trace)
import System.Environment (getArgs)

type Coord = (Int, Int)

b2i :: Bool -> Int
b2i False = 0
b2i True = 1

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

main = do
  args <- getArgs
  raw_text <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let grid = toUArray $ lines raw_text :: Grid

  -- print $ grid
  -- print $ grid ! (2, 3)
  -- let region_c = floodFill grid [] (2, 3)
  -- print region_c
  -- print $ sum $ map (corners grid region_c) region_c
  -- print $ price grid $ region_c
  print $ sum $ solve grid

-- price = number of sides * area = number of corners * area
price :: Grid -> [Coord] -> Int
price g s =
  let area = length s
      sides = sum (map (corners g s) s)
   in area * sides

-- number of valid corners at on position in a region
-- assumes that x `elem` region
corners :: Grid -> [Coord] -> Coord -> Int
corners g region (r, c) =
  let reg = Set.fromList region
      top = (r - 1, c)
      left = (r, c - 1)
      right = (r, c + 1)
      bottom = (r + 1, c)
      topLeft = (r - 1, c - 1)
      topRight = (r - 1, c + 1)
      bottomLeft = (r + 1, c - 1)
      bottomRight = (r + 1, c + 1)
      cornerOutsideTopLeft = Set.notMember top reg && Set.notMember left reg
      cornerOutsideTopRight = Set.notMember top reg && Set.notMember right reg
      cornerOutsideBottomLeft = Set.notMember bottom reg && Set.notMember left reg
      cornerOutsideBottomRight = Set.notMember bottom reg && Set.notMember right reg
      cornerInsideTopLeft = Set.member top reg && Set.member left reg && Set.notMember topLeft reg
      cornerInsideTopRight = Set.member top reg && Set.member right reg && Set.notMember topRight reg
      cornerInsideBottomLeft = Set.member bottom reg && Set.member left reg && Set.notMember bottomLeft reg
      cornerInsideBottomRight = Set.member bottom reg && Set.member right reg && Set.notMember bottomRight reg
   in b2i cornerOutsideBottomLeft + b2i cornerOutsideBottomRight + b2i cornerOutsideTopLeft + b2i cornerOutsideTopRight + b2i cornerInsideBottomLeft + b2i cornerInsideBottomRight + b2i cornerInsideTopLeft + b2i cornerInsideTopRight

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

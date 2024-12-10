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
  -- print $ grid
  -- print $ map (\(a, b) -> (fst a, fst b)) $ pairs grid '0'
  
  print $ map (uncurry linearDiff) $ pairs grid '0'

linearDiff :: Entry -> Entry -> (Int, Int)
linearDiff ((ar, ac), _) ((br, bc), _) = (br - ar, bc - ac)

-- gets all possible pairs of a frequency
-- bidirectional, so both (a,b) AND (b,a) will be included)
pairs :: Grid -> Frequency -> [(Entry, Entry)]
pairs grid freq =
  let coords = filter (\((r, c), v) -> v == Antenna freq) grid
   in [(a, b) | a <- coords, b <- coords, a /= b]

import System.Environment (getArgs)

type Frequency = Char

data Cell = Free | Antinode Frequency | Antenna Frequency deriving (Show, Eq)

type Coord = (Int, Int)

type Grid = [[Cell]]

main = do
  args <- getArgs
  raw_text <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let grid = (map . map) (\x -> if x == '.' then Free else Antenna x) $ lines raw_text
  print $ grid

-- gets all possible pairs of a frequency
-- bidirectional, so both (a,b) AND (b,a) will be included)
pairs :: Grid -> Frequency -> [Coord]
pairs grid freq = []

import System.Environment (getArgs)

data CellValue = Free | Antinode Char | Antenna Char deriving (Show, Eq)

type Cell = [CellValue]

type Coord = (Int , Int)

main = do
  args <- getArgs
  raw_text <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let grid = (map . map) (\x -> if x == '.' then Free else Antenna x) $ lines raw_text
  print $ grid

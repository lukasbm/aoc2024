import System.Environment (getArgs)

data Sector = Free {size :: Int} | File {id :: Int, size :: Int} deriving (Eq)

instance Show Sector where
  show (Free size) = replicate size '.'
  show (File id size) = replicate size (head $ show id)

-- bool = True means parse File, false means parse Free
-- The int references the id to be used in a new file
-- the last int is the sector to be parsed
parse :: Bool -> Int -> [Int] -> [Sector]
parse _ _ [] = []
parse True id (s : xs) = File id s : parse False (id + 1) xs
parse False id (s : xs) = Free s : parse True id xs
main = do
  args <- getArgs
  raw_text <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let num = map (read . pure :: Char -> Int) raw_text
  print $ num
  print $ parse True 0 num

import System.Environment (getArgs)

-- a sector is either free or a file block (with a file id)
data Sector = Free | Block Int deriving (Eq)

instance Show Sector where
  show Free = "."
  show (Block id) = show id

-- bool = True means parse File, false means parse Free
-- The int references the id to be used in a new file
-- the last int is the sector to be parsed
parse :: Bool -> Int -> [Int] -> [Sector]
parse _ _ [] = []
parse True id (s : xs) = replicate s (Block id) ++ parse False (id + 1) xs
parse False id (s : xs) = replicate s Free ++ parse True id xs

defrag :: [Sector] -> [Sector]
defrag [] = []
defrag [x] = [x]
defrag xs =
  let -- blocks to weave (needs to be woven into Free places) by going backwards over the list
      weaving = reverse $ filter (/= Free) xs
   in helper xs weaving
  where
    -- list to process, weaving
    helper :: [Sector] -> [Sector] -> [Sector]
    helper [] wss = wss
    helper _ [] = []
    helper [Free] wss = wss
    helper [Block id] _ = [Block id]
    helper (Free : xs) (w : ws) = w : helper (init xs) ws -- we included a weave, can remove from end of list
    helper (Block id : xs) wss = Block id : helper xs (init wss) -- in case a block in the regular list is present, this is one element we don't need to weave (remove it from the end of wss)

checksum :: [Sector] -> Int
checksum xs =
  let clean = map (\(Block id) -> id) $ takeWhile (/= Free) xs
   in sum $ zipWith (*) clean [0 ..]

main = do
  args <- getArgs
  raw_text <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let num = parse True 0 $ map (read . pure :: Char -> Int) raw_text
  -- print $ length num
  -- print $ num
  -- print $ defrag num
  print $ checksum $ defrag num

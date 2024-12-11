import System.Environment (getArgs)
import Prelude hiding (id)

data Block = Free {size :: Int} | File {id :: Int, size :: Int} deriving (Eq)

-- should have used a datatype, but here we represent free with -1
blocksToSectors :: [Block] -> [Int]
blocksToSectors [] = []
blocksToSectors (Free size : xs) = replicate size (-1) ++ blocksToSectors xs
blocksToSectors (File id size : xs) = replicate size id ++ blocksToSectors xs

-- bool = True means parse File, false means parse Free
-- The int references the id to be used in a new file
-- the last int is the sector to be parsed
parse :: Bool -> Int -> [Int] -> [Block]
parse _ _ [] = []
parse True id (s : xs) = File id s : parse False (id + 1) xs
parse False id (s : xs) = Free s : parse True id xs

defrag :: [Block] -> [Block]
defrag [] = []
defrag [x] = [x]
defrag xs =
  let -- blocks to weave (needs to be woven into Free places) by going backwards over the list
      weaving = reverse $ filter (\x -> case x of Free _ -> False; File _ _ -> True) xs
   in helper xs weaving
  where
    -- list to process, weaving
    helper :: [Block] -> [Block] -> [Block]
    helper [] wss = []
    helper xss [] = xss -- actually done (happy case)
    -- if i weave something i need to remove, it needs to be removed from the end of xs and replaced with free!
    helper (Free xsize : xs) (w@(File wid wsize) : ws) =
      if wsize <= xsize -- check if can weave
        then [w] ++ helper (Free (xsize - wsize) : init xs) ws ++ [Free wsize] -- actually replace!
        else Free xsize : helper xs ws -- can't replace, but need to make sure the block stays where it
    helper (x@(File id xsize) : xs) wss = x : helper xs wss -- in case a block in the regular list is present, this is one element we don't need to weave (remove it from the end of wss)

checksum :: [Int] -> Int
checksum xs =
  let clean = filter (>= 0) xs
   in sum $ zipWith (*) clean [0 ..]

main = do
  args <- getArgs
  raw_text <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let num = parse True 0 $ map (read . pure :: Char -> Int) raw_text
  print $ blocksToSectors $ num
  print $ blocksToSectors $ defrag num
  print $ checksum $ blocksToSectors $ defrag num

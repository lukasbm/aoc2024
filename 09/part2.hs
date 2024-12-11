import System.Environment (getArgs)
import Prelude hiding (id)

data Block = Free {size :: Int} | File {id :: Int, size :: Int} deriving (Eq, Show)

-- should have used a datatype, but here we represent free with 0
blocksToSectors :: [Block] -> [Int]
blocksToSectors [] = []
blocksToSectors (Free size : xs) = replicate size 0 ++ blocksToSectors xs
blocksToSectors (File id size : xs) = replicate size id ++ blocksToSectors xs

-- bool = True means parse File, false means parse Free
-- The int references the id to be used in a new file
-- the last int is the sector to be parsed
parse :: Bool -> Int -> [Int] -> [Block]
parse _ _ [] = []
parse True id (s : xs) = File id s : parse False (id + 1) xs
parse False id (s : xs) = Free s : parse True id xs

-- checks if the block could actually be moved
-- assumed the block is still present in the full list!
spaceOnLeft :: [Block] -> Block -> Bool
spaceOnLeft xs x@(File xid xsize) =
  let left = takeWhile (/= x) xs
   in any fits left
  where
    fits (File yid ysize) = False
    fits (Free ysize) = ysize >= xsize

replace :: (Eq a) => a -> a -> [a] -> [a]
replace a b = map $ \c -> if c == a then b else c

-- finds the first-best spot to place it in.
replaceLeft :: [Block] -> Block -> [Block]
replaceLeft [] _ = []
replaceLeft [a] _ = [a]
replaceLeft (x@(Free xsize) : xs) y@(File yid ysize) =
  if xsize >= ysize
    then y : Free (xsize - ysize) : replace y (Free ysize) xs
    else x : replaceLeft xs y
replaceLeft (x : xs) y = x : replaceLeft xs y

-- NOTE: in this exercise i CAN NOT build the final output left to right!!!
defrag :: [Block] -> [Block]
defrag [] = []
defrag [x] = [x]
defrag xs =
  let -- blocks to weave (needs to be woven into Free places) by going backwards over the list
      weaving = reverse $ filter (\x -> case x of Free _ -> False; File _ _ -> True) xs
   in helper xs weaving
  where
    helper xs [] = xs
    helper xs (w@(File wid wsize) : ws) =
      if spaceOnLeft xs w
        then helper (replaceLeft xs w) ws
        else helper xs ws

checksum :: [Int] -> Int
checksum xs =
  let clean = filter (>= 0) xs
   in sum $ zipWith (*) clean [0 ..]

main = do
  args <- getArgs
  raw_text <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let num = parse True 0 $ map (read . pure :: Char -> Int) raw_text

  -- print $ blocksToSectors $ num
  -- print $ blocksToSectors $ defrag num
  print $ checksum $ blocksToSectors $ defrag num

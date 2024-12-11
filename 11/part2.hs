import Data.Bits (Bits (xor))
import Data.IntMap qualified as Map
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

applyRules :: Int -> [Int]
applyRules x
  | x == 0 = [1]
  | even pow = let (a, b) = x `divMod` (10 ^ (pow `div` 2)) in [a, b]
  | otherwise = [x * 2024]
  where
    -- pow is the nearest power of then so: pow 25000 = [1,10,100,1000,10000]
    pow = length . takeWhile (<= x) $ iterate (* 10) 1

frequency :: [Int] -> Map.IntMap Int
frequency = Map.fromListWith (+) . map (,1)

-- same way to handle conflicts when joining as in fromList
-- essentially apply rules to every key in the map, then unites the maps back together
stepMemo :: Map.IntMap Int -> Map.IntMap Int
stepMemo xm = Map.unionsWith (+) [(* n) <$> frequency (applyRules x) | (x, n) <- Map.toList xm]

main = do
  args <- getArgs
  raw_text <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let stones = map read $ words raw_text :: [Int]
  -- maps input to the amount of numbers it will split to
  -- the with part is the operation (+) to be executed on a key conflict.
  let stones_map = frequency stones :: Map.IntMap Int

  print $ sum $ map snd $ Map.toList $ (!! 75) $ iterate stepMemo stones_map

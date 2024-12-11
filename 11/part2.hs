import Data.Bits (Bits (xor))
import Data.IntMap qualified as Map
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

-- NOTE: the claim about order does not matter!!
-- we can represent the problem as a nested multiset where ordering does not matter
-- ideas: lazy list, map, memo-combinators, multi-set

applyRules :: Int -> [Int]
applyRules x
  | x == 0 = [1]
  | even pow = let (a, b) = x `divMod` (10 ^ (pow `div` 2)) in [a, b]
  | otherwise = [x * 2024]
  where
    -- pow is the nearest power of then so: pow 25000 = [1,10,100,1000,10000]
    pow = length . takeWhile (<= x) $ iterate (* 10) 1

frequencyMap :: [Int] -> Map.IntMap Int
frequencyMap = Map.fromListWith (+) . map (,1)

-- same way to handle conflicts when joining as in fromList
-- essentially apply rules to every key in the map, then unites the maps back together
stepMemo :: Map.IntMap Int -> Map.IntMap Int
stepMemo xm = Map.unionsWith (+) [(* n) <$> frequencyMap (applyRules x) | (x, n) <- Map.toList xm]

main = do
  args <- getArgs
  raw_text <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let stones = map read $ words raw_text :: [Int]
  -- maps input to the amount of numbers it will split to
  -- the with part is the operation (+) to be executed on a key conflict.
  let stones_map = frequencyMap stones :: Map.IntMap Int

  print $ stones
  print $ stones_map
  print $ stepMemo stones_map
  print $ "idk"
  print $ [(* n) <$> frequencyMap (applyRules x) | (x, n) <- Map.toList stones_map]
  print $ [frequencyMap (applyRules x) | (x, n) <- Map.toList stones_map]
  print $ [frequencyMap $ map (*n) (applyRules x) | (x, n) <- Map.toList stones_map]

  print $ sum $ map snd $ Map.toList $ (!! 25) $ iterate stepMemo stones_map

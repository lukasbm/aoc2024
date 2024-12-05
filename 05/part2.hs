import Data.Bifunctor (bimap)
import Data.List (sortBy)

type Rule = (Int, Int)

type Update = [Int]

split :: Char -> String -> [String]
split _ [] = []
split sep x =
  let (a, b) = break (== sep) x
   in a : if null b then [] else split sep (tail b)

overlaps :: [a] -> [(a, a)]
overlaps x = zip x (tail x)

takeMiddle :: [a] -> a
takeMiddle [] = error "needs to have at least 1 elem"
takeMiddle [x] = x
takeMiddle [a, b] = error "needs to have an odd number of elems"
takeMiddle (a : xs) = takeMiddle (init xs)

main = do
  raw_text <- getContents
  let rules = map (bimap read (read . tail) . break (== '|')) $ takeWhile (/= "") (lines raw_text) :: [Rule]
  let updates = map (map (read :: String -> Int) . split ',') $ tail $ dropWhile (/= "") $ lines raw_text :: [[Int]]

  print $ sum $ map takeMiddle $ filter (\x -> correctlyOrdered rules x []) updates

correctlyOrdered :: [Rule] -> Update -> Update -> Bool
correctlyOrdered _ [] _ = True
correctlyOrdered [] _ _ = True
correctlyOrdered rules (u : us) processed =
  let relevantRules = filter (\(a, b) -> a == u) rules
   in all (\(a, b) -> b `notElem` processed) relevantRules && correctlyOrdered rules us (u : processed)

sortUpdate :: [Rule] -> Update -> Update
sortUpdate rules = sortBy comparePage
  where
    -- TODO: work here!
    comparePage :: Int -> Int -> Ordering
    comparePage a b = GT -- need to find a path where its something like (a, x) -> (x,x) -> (x,b)   [tuples represent the rules here. They form a DAG]

-- relevant for reordering [61,13,29]?
-- 47|61
-- 75|61
-- 61|53
-- 61|29
-- 97|61
-- 61|13
-- 75|13
-- 53|13
-- 47|13
-- 29|13
-- 97|13
-- 97|29
-- 53|29
-- 75|29
-- 47|29
-- NOTE: the rules always form a DAG!!! in this case its
-- 46 -> 61 -> 53 -> 13 or 75 -> 29 -> 13 ....
-- 13 is the final element!

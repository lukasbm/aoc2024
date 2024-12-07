import Data.Bifunctor (bimap)
import Data.List (delete, find, sortBy)
import Data.Maybe (isJust)

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

  -- print $ sum $ map takeMiddle $ filter (\x -> correctlyOrdered rules x []) updates
  print $ correctlyOrdered rules [61, 13, 29] []
  print $ sortUpdate rules [61, 13, 29]
  print $ sortUpdate rules [97, 13, 75, 29, 47]

correctlyOrdered :: [Rule] -> Update -> Update -> Bool
correctlyOrdered _ [] _ = True
correctlyOrdered [] _ _ = True
correctlyOrdered rules (u : us) processed =
  let relevantRules = filter (\(a, b) -> a == u) rules
   in all (\(a, b) -> b `notElem` processed) relevantRules && correctlyOrdered rules us (u : processed)

-- need to find a path where its something like (a, x) -> (x,x) -> (x,b)   [tuples represent the rules here. They form a DAG]
hasPath :: [Rule] -> Int -> Int -> Bool
hasPath rules a b = walk rules a
  where
    walk :: [Rule] -> Int -> Bool
    walk rules startPage =
      let relevantRules = filter (\(x, y) -> x == startPage) rules
       in any (\(x, y) -> y == b) relevantRules || any (\(x, y) -> walk rules y) relevantRules -- first is checking if we reached the goal, else is breath search

sortUpdate :: [Rule] -> Update -> Update
sortUpdate rules = sortBy comparePage
  where
    -- sorts two pages from an update given the rules
    -- if there is a path from a to b -> then a LT b
    -- a == b -> then A EQ b
    -- if there is no path from a to b -> then a GT b
    comparePage :: Int -> Int -> Ordering
    comparePage a b
      | a == b = EQ
      | hasPath rules a b = LT
      | otherwise = GT

import Data.Bifunctor (bimap)
import Data.List (delete, find, sortBy)
import Data.Maybe (isJust)
import System.Environment (getArgs)

type Rule = (Int, Int)

type Update = [Int]

-- split a string at a char
-- drops the char from the results!
split :: Char -> String -> [String]
split _ [] = []
split sep x =
  let (a, b) = break (== sep) x
   in a : if null b then [] else split sep (tail b)

-- returns overlapped tuples
-- e.g. overlaps [1..4] = [(1,2), (2,3), (3,4)]
overlaps :: [a] -> [(a, a)]
overlaps x = zip x (tail x)

-- take the middle element of an odd-sized list
takeMiddle :: [a] -> a
takeMiddle [] = error "needs to have at least 1 elem"
takeMiddle [x] = x
takeMiddle [a, b] = error "needs to have an odd number of elems"
takeMiddle (a : xs) = takeMiddle (init xs)

main = do
  args <- getArgs
  raw_text <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let rules = map (bimap read (read . tail) . break (== '|')) $ takeWhile (/= "") (lines raw_text) :: [Rule]
  let updates = map (map (read :: String -> Int) . split ',') $ tail $ dropWhile (/= "") $ lines raw_text :: [[Int]]

  -- print $ correctlyOrdered rules [61, 13, 29] []
  -- print $ sortUpdate rules [61, 13, 29]
  -- print $ sortUpdate rules [97, 13, 75, 29, 47]

  -- FIXME: 6479 is too high! 6000 is too small!
  print $ filter (\x -> not $ correctlyOrdered rules x []) updates
  print $ sum $ map (takeMiddle . sortUpdate rules) $ filter (\x -> not $ correctlyOrdered rules x []) updates

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

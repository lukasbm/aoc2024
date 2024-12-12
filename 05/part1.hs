import Data.Bifunctor (bimap)
import System.Environment (getArgs)

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
  args <- getArgs
  raw_text <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let rules = map (bimap read (read . tail) . break (== '|')) $ takeWhile (/= "") (lines raw_text) :: [Rule]
  let updates = map (map (read :: String -> Int) . split ',') $ tail $ dropWhile (/= "") $ lines raw_text :: [[Int]]

  -- test with test_part1.txt
  -- print $ correctlyOrdered rules [97, 13, 75, 29, 47] [] -- should false!
  -- print $ correctlyOrdered rules [75, 47, 61, 53, 29] [] -- should be true
  -- print $ map (\x -> correctlyOrdered rules x []) updates -- should be T,T,T,F,F,F
  -- print $ filter (\x -> correctlyOrdered rules x []) updates

  print $ sum $ map takeMiddle $ filter (\x -> correctlyOrdered rules x []) updates

correctlyOrdered :: [Rule] -> Update -> Update -> Bool
correctlyOrdered _ [] _ = True
correctlyOrdered [] _ _ = True
correctlyOrdered rules (u : us) processed =
  let relevantRules = filter (\(a, b) -> a == u) rules
   in all (\(a, b) -> b `notElem` processed) relevantRules && correctlyOrdered rules us (u : processed)

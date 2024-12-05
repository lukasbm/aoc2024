import Data.Bifunctor (bimap)

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
  print $ filter (correctlyOrdered rules []) updates
  print $ sum $ map takeMiddle $ filter (correctlyOrdered rules []) updates

correctlyOrdered :: [Rule] -> Update -> Update -> Bool
correctlyOrdered _ [] _ = True
correctlyOrdered [] _ _ = True
correctlyOrdered rules (u : us) processed =
  let relevantRules = filter (\(a, b) -> a == u) rules
   in any ((`elem` processed) . snd) relevantRules && correctlyOrdered rules us (u : processed)

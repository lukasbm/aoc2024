parseReport :: [String] -> [Int]
parseReport = map read

main = do
  raw_txt <- readFile "input.txt"
  let reports = map (parseReport . words) $ lines raw_txt
  print $ foldl (\acc p -> (if p then acc + 1 else acc)) 0 $ map evaluateReport reports

-- The levels are either all increasing or all decreasing.
-- Any two adjacent levels differ by at least one and at most three.
evaluateReport :: [Int] -> Bool
evaluateReport [] = False
evaluateReport ls =
  let staggered = zip ls (tail ls)
   in foldl (\acc (a, b) -> acc && abs (a - b) <= 3 && abs (a - b) >= 1) True staggered
        && ( foldl (\acc (a, b) -> acc && (a > b)) True staggered
               || foldl (\acc (a, b) -> acc && (a < b)) True staggered
           )

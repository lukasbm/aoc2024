parseReport :: [String] -> [Int]
parseReport = map read

main = do
  raw_txt <- readFile "test_part1.txt"
  let reports = map (parseReport . words) $ lines raw_txt
  print $ map evaluateReport reports

-- The levels are either all increasing or all decreasing.
-- Any two adjacent levels differ by at least one and at most three.
evaluateReport :: [Int] -> Bool
evaluateReport [] = False
evaluateReport ls =
  let staggered = zip ls (tail ls)
   in foldl (\acc (a, b) -> acc && (abs (a - b) <= 3)) True staggered
        || foldl (\acc (a, b) -> acc && (a > b)) True staggered
        || foldl (\acc (a, b) -> acc && (a < b)) True staggered

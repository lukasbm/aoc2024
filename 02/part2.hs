import System.Environment (getArgs)

parseReport :: [String] -> [Int]
parseReport = map read

main = do
  args <- getArgs
  raw_txt <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let reports = map (parseReport . words) $ lines raw_txt
  let ratings = map evaluateReportTestAll reports
  print $ foldl (\acc p -> (if p then acc + 1 else acc)) 0 ratings

removeLevel :: [Int] -> Int -> [Int]
removeLevel ls i = take i ls ++ drop (i + 1) ls

evaluateReportTestAll :: [Int] -> Bool
evaluateReportTestAll ls =
  let perms = map (removeLevel ls) [0 .. (length ls - 1)]
   in foldl (\acc l -> acc || evaluateReport l) False perms

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

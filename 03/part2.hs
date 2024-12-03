{-# LANGUAGE OverloadedStrings #-}

-- I did a dirty global install: `cabal install --lib regex-compat`
import Data.List (intercalate)
import Text.Regex (Regex, matchRegex, matchRegexAll, mkRegex)

findAllMatches :: Regex -> String -> [String]
findAllMatches pattern text = case matchRegexAll pattern text of
  Just (before, match, after, subs) -> filter (/= "") subs ++ findAllMatches pattern after
  Nothing -> []

parseMatches :: [String] -> [(Int, Int)]
parseMatches [] = []
parseMatches ("do" : x : y : rest) = (read x, read y) : parseMatches rest
parseMatches ("don't" : x : y : rest) = parseMatches rest
parseMatches (x : y : rest) = parseMatches rest
parseMatches _ = error "list not fully specified"

patternEq = mkRegex "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)|(do)\\(\\)|(don't)\\(\\)"

main :: IO ()
main = readFile "input.txt" >>= print . sum . solve . intercalate "" . lines

-- main = do
--   raw_txt <- readFile "test_part2.txt"
--   let a = findAllMatches patternEq $ intercalate "" $ lines raw_txt
--   print $ parseMatches ("do" : a)

solve :: String -> [Int]
solve text = map (\(x, y) -> x * y) $ parseMatches ("do" : findAllMatches patternEq text)

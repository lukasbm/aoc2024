{-# LANGUAGE OverloadedStrings #-}

-- I did a dirty global install: `cabal install --lib regex-compat`
import Data.List (intercalate)
import Text.Regex (Regex, matchRegex, matchRegexAll, mkRegex)

findAllMatches :: Regex -> String -> [String]
findAllMatches pattern text = case matchRegexAll pattern text of
  Just (before, match, after, subs) -> subs ++ findAllMatches pattern after
  Nothing -> []

parseMatches :: [String] -> [(Int, Int)]
parseMatches [] = []
parseMatches (x : y : rest) = (read x, read y) : parseMatches rest
parseMatches _ = error "list contains uneven number of elements"

patternEq = mkRegex "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"

main :: IO ()
main = readFile "input.txt" >>= print . sum . solve . intercalate "" . lines

solve :: String -> [Int]
solve text = map (\(x, y) -> x * y) $ parseMatches $ findAllMatches patternEq text

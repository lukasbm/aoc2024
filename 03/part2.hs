{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use uncurry" #-}

-- I did a dirty global install: `cabal install --lib regex-compat`
import Data.List (intercalate)
import System.Environment (getArgs)
import Text.Regex (Regex, matchRegex, matchRegexAll, mkRegex)

findAllMatches :: Regex -> String -> [String]
findAllMatches pattern text = case matchRegexAll pattern text of
  Just (before, match, after, subs) -> filter (/= "") subs ++ findAllMatches pattern after
  Nothing -> []

parseMatches :: [String] -> Bool -> [(Int, Int)]
parseMatches [] a = []
parseMatches ("do" : x : y : rest) a = (read x, read y) : parseMatches rest True
parseMatches ("don't" : x : y : rest) a = parseMatches rest False
parseMatches (x : y : rest) True = (read x, read y) : parseMatches rest True
parseMatches (x : y : rest) False = parseMatches rest False
parseMatches _ a = error "list not fully specified"

patternEq = mkRegex "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)|(do)\\(\\)|(don't)\\(\\)"

main :: IO ()
main = do
  args <- getArgs
  raw_text <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  print $ sum $ solve $ intercalate "" $ lines raw_text

solve :: String -> [Int]
solve text = map (\(x, y) -> x * y) $ parseMatches (findAllMatches patternEq text) True

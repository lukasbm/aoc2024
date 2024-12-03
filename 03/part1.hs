{-# LANGUAGE OverloadedStrings #-}

-- I did a dirty global install: `cabal install --lib regex-tdfa`
-- maybe regex-compat would have been better ....
import Data.List (intercalate)
import Text.Regex.TDFA (AllTextMatches, getAllTextMatches, (=~))

findAllOccurrences :: String -> String -> [String]
findAllOccurrences pattern text = getAllTextMatches (text =~ pattern :: AllTextMatches [] String)

-- pattern1 = "mul\\([0-9]{1,3},[0-9]{1,3}\\)"
pattern1 = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"

main :: IO ()
main = readFile "test_part1.txt" >>= print . solve . intercalate "" . lines

solve :: String -> [String]
solve text = findAllOccurrences pattern1 text

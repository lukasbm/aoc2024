import Data.List (intercalate)
import Text.Regex


regex = "mul\\(\\d{1,3},\\d{1,3}\\)"


main = readFile "input.txt" >>= print . intercalate "" . lines

import Data.Char (isDigit)
import System.Environment (getArgs)

-- X and Y moves
type Move = (Int, Int)

data Machine = Machine
  { buttonA :: Move,
    buttonB :: Move,
    prize :: Move
  }
  deriving (Show)

parse :: [String] -> [Machine]
parse [] = []
parse txt = let nextBlock = take 3 txt in go nextBlock : parse (drop 4 txt)
  where
    go :: [String] -> Machine
    go [a, b, p] =
      let button_a = map (read . filter isDigit) $ drop 2 $ words a :: [Int]
          button_b = map (read . filter isDigit) $ drop 2 $ words b :: [Int]
          prize = map (read . filter isDigit) $ drop 1 $ words p :: [Int]
       in Machine (head button_a, last button_a) (head button_b, last button_b) (head prize, last prize)
    go xs = error ("invalid input block" ++ show xs)

main = do
  args <- getArgs
  raw <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let machines = parse $ lines raw
  print machines
  print $ solveMachine $ head machines
  print $ calcPrice $ solveMachine $ head machines

calcPrice :: Maybe (Int, Int) -> Int
calcPrice (Just (a, b)) = 3 * a + b
calcPrice Nothing = 0

-- A = 3 token
-- B = 1 token
-- don't press more than 100 buttons on any machine.
-- not all claw machines are solvable!
-- returns Just (number of a presses, number of b presses) or Nothing if unsolvable
solveMachine :: Machine -> Maybe (Int, Int)
solveMachine m = Nothing

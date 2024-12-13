import Data.Array
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
  print $ head machines
  let m = head machines
  print $ (listArray ((1, 1), (2, 2)) [fst $ buttonA m, fst $ buttonB m, snd $ buttonA m, snd $ buttonB m] :: Array (Int, Int) Int)

-- print $ solveMachine $ head machines
-- print $ calcPrice $ solveMachine $ head machines

calcPrice :: Maybe (Int, Int) -> Int
calcPrice (Just (a, b)) = 3 * a + b
calcPrice Nothing = 0

-- A = 3 token
-- B = 1 token
-- don't press more than 100 buttons on any machine.
-- not all claw machines are solvable!
-- returns Just (number of a presses, number of b presses) or Nothing if unsolvable
--
-- the task is a simple linear system of equations
-- A.X * a + B.X * b = P.X
-- A.Y * a + B.Y * b = P.Y
-- A.X means the X move on button A for that machine
--
-- we don't have to integrate the costs into the system of equations,
-- as there is either exactly one solution (if det != 0) or no solution.
solveMachine :: Machine -> Maybe (Int, Int)
solveMachine m =
  let -- matrix = a11 = A.X , a12 = B.X , a21 = A.Y , a22 = B.Y
      matrix = listArray ((1, 1), (2, 2)) [fst $ buttonA m, fst $ buttonB m, snd $ buttonA m, snd $ buttonB m] :: Array (Int, Int) Int
      -- det = a11 a22 - a12 a21
      determinant = (matrix ! (1, 1)) * (matrix ! (2, 2)) - (matrix ! (1, 2)) * (matrix ! (2, 1))
   in if determinant == 0
        then Nothing
        else Just (1, 1)

import Data.Array
import Data.Char (isDigit)
import Debug.Trace (trace)
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
  -- print $ map solveMachine machines
  print $ sum $ map (calcPrice . solveMachine) machines

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
      solution = solveSOE matrix (prize m)
   in if determinant == 0 || fst solution > 100 || snd solution > 100
        then Nothing
        else Just solution

-- solve a 2x2 system of equations
-- find x in ax=b
-- in case the column vectors of a are linearly dependent, we only solve the equations for button B as it is cheaper
-- in the input there are no values of 0, so we don't need to handle that case.
solveSOE :: Array (Int, Int) Int -> (Int, Int) -> (Int, Int)
solveSOE a b =
  let -- mult first eq by a21 and second eq by a11
      new_a = listArray (bounds a) [a ! (1, 1) * a ! (2, 1), a ! (1, 2) * a ! (2, 1), a ! (2, 1) * a ! (1, 1), a ! (2, 2) * a ! (1, 1)]
      new_b = (fst b * a ! (2, 1), snd b * a ! (1, 1))
      -- take the difference of equations. first term eliminates x1
      eq_diff = abs $ new_a ! (2, 2) - new_a ! (1, 2)
      res_diff = abs $ snd new_b - fst new_b
      x2 = res_diff `div` eq_diff
      x1 = (fst b - (a ! (1, 2) * x2)) `div` a ! (1, 1)
      -- case linearly dependent
      linearlyDependent = gcd (a ! (1, 1)) (a ! (1, 2)) /= 1 && gcd (a ! (2, 1)) (a ! (2, 2)) /= 1
      -- only solve for b
      x1_simple = fst b `div` (a ! (1, 2))
      x2_simple = snd b `div` (a ! (2, 2))
   in if linearlyDependent then (abs x1_simple, abs x2_simple) else (abs x1, abs x2)

-- FIXME: 32840 too low!
-- FIXME: 40576 too high!

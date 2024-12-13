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
      a = listArray ((1, 1), (2, 2)) [fromIntegral $ fst (buttonA m), fromIntegral $ fst (buttonB m), fromIntegral $ snd (buttonA m), fromIntegral $ snd (buttonB m)] :: Array (Int, Int) Double
      det_a = determinant a
      a1 = listArray (bounds a) [fromIntegral $ fst $ prize m, a ! (1, 2), fromIntegral $ snd $ prize m, a ! (2, 2)] :: Array (Int, Int) Double
      a2 = listArray (bounds a) [a ! (1, 1), fromIntegral $ fst $ prize m, a ! (2, 1), fromIntegral $ snd $ prize m] :: Array (Int, Int) Double
      det_a1 = determinant a1
      det_a2 = determinant a2
      -- determinant_inv = 1 `div` determinant
      -- matrix_inv = listArray (bounds matrix) [determinant_inv * matrix ! (2, 2), -(determinant_inv * matrix ! (1, 2)), -(determinant_inv * matrix ! (2, 1)), determinant_inv * matrix ! (1, 1)]
      -- determinant is never 0 in the input ...
      x1 = det_a1 / det_a
      x2 = det_a2 / det_a
   in if x1 > 100 || x2 > 100
        then Nothing
        else Just (round x1, round x2)

-- det = a11 a22 - a12 a21
determinant :: (Num a) => Array (Int, Int) a -> a
determinant matrix = (matrix ! (1, 1)) * (matrix ! (2, 2)) - (matrix ! (1, 2)) * (matrix ! (2, 1))

-- FIXME: 32840 too low!
-- FIXME: 40576 too high!
-- FIXME: 33501 is wrong!
-- FIXME: 39646 is wrong!
-- FIXME: 39675 is wrong!

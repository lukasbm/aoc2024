import System.Environment (getArgs)

type Equation = (Int, [Int])

b2i :: Bool -> Int
b2i False = 0
b2i True = 1

parseEquation :: String -> Equation
parseEquation s = let (l, r) = break (':' ==) s in (read l, map read . words . tail $ r)

main = do
  args <- getArgs
  raw_text <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let equations = map parseEquation $ lines raw_text
  -- print $ map eval equations
  print $ sum $ map fst $ filter eval equations

-- checks if equation can be solved using add and multiply
-- evaluation is always left to right
-- current approach: simple backtracking
eval :: Equation -> Bool
eval (test, []) = True
eval (test, [a]) = a == test
eval (test, a : b : xs) = eval (test, a * b : xs) || eval (test, a + b : xs)

-- i think this can be represented in a comonad or zipper?

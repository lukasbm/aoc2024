import System.Environment (getArgs)

type Equation = (Int, [Int])

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
eval (test, a : b : xs) = eval (test, a * b : xs) || eval (test, a + b : xs) || eval (test, a `intjoin` b : xs)
  where
    intjoin :: Int -> Int -> Int
    intjoin a b = read (show a <> show b)

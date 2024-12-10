import System.Environment (getArgs)

main = do
  args <- getArgs
  raw_text <- if length args == 1 then readFile (head args) else error "usage: ./program <file>"
  let num = read raw_text :: Int
  print $ num

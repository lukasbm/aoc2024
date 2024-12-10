import System.Environment (getArgs)

main = do
  args <- getArgs
  file <- readFile $ head args
  print $ file

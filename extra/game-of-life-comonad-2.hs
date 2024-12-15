{-# LANGUAGE TypeFamilies #-}

-- cabal install --lib vector
-- cabal install --lib comonad
-- cabal install --lib distributive
-- cabal install --lib adjunctions

import Control.Comonad (Comonad (..))
import Control.Comonad.Representable.Store (Store (..), StoreT (..), experiment, store)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Data.Bool (bool)
import Data.Distributive (Distributive (..))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Rep (Representable (..), distributeRep)
import Data.Vector (Vector, generate, (!))

-- this is another example of comonads, but using vecotrs to represent an infinite grid!


tickTime :: Int
tickTime = 200000

start :: Grid
start =
  mkGrid $
    glider `at` (0, 0)
      ++ beacon `at` (15, 5)

main :: IO ()
main = forM_ (iterate (step basicRule) start) $ \grid -> do
  putStr "\ESC[2J" -- Clear terminal screen
  putStrLn (render grid)
  threadDelay tickTime

type Coord = (Int, Int)

type Grid = Store (Compose Vector Vector) Bool

type Rule = Grid -> Bool

instance Distributive Vector where
  distribute = distributeRep

instance Representable Vector where
  type Rep Vector = Int
  index v i = v ! (i `mod` gridSize)
  tabulate = generate gridSize

gridSize :: Int
gridSize = 20

neighbourCoords :: [Coord]
neighbourCoords = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]

addCoords :: Coord -> Coord -> Coord
addCoords (x, y) (x', y') = (x + x', y + y')

basicRule :: Rule
basicRule g = numNeighboursAlive == 3 || (alive && numNeighboursAlive == 2)
  where
    alive = extract g
    neighbours = experiment (at neighbourCoords) g
    numNeighboursAlive = length (filter id neighbours)

step :: Rule -> Grid -> Grid
step = extend

render :: Grid -> String
render (StoreT (Identity (Compose g)) _) = foldMap ((++ "\n") . foldMap (bool "." "#")) g

mkGrid :: [Coord] -> Grid
mkGrid xs = store (`elem` xs) (0, 0)

at :: [Coord] -> Coord -> [Coord]
coords `at` origin = map (addCoords origin) coords

glider, blinker, beacon :: [Coord]
glider = [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
blinker = [(0, 0), (1, 0), (2, 0)]
beacon = [(0, 0), (1, 0), (0, 1), (3, 2), (2, 3), (3, 3)]

{-# LANGUAGE FlexibleContexts #-}

import Control.Monad (foldM)
import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.Set as S

dijkstra :: (Ix v, Num w, Ord w, Bounded w) => v -> v -> Array v [(v, w)] -> (Array v w, Array v v)
dijkstra src invalid_index adj_list = runST $ do
  min_distance <- newSTArray b maxBound
  writeArray min_distance src 0
  previous <- newSTArray b invalid_index
  let aux vertex_queue =
        case S.minView vertex_queue of
          Nothing -> return ()
          Just ((dist, u), vertex_queue') ->
            let edges = adj_list ! u
                f vertex_queue (v, weight) = do
                  let dist_thru_u = dist + weight
                  old_dist <- readArray min_distance v
                  if dist_thru_u >= old_dist
                    then
                      return vertex_queue
                    else do
                      let vertex_queue' = S.delete (old_dist, v) vertex_queue
                      writeArray min_distance v dist_thru_u
                      writeArray previous v u
                      return $ S.insert (dist_thru_u, v) vertex_queue'
             in foldM f vertex_queue' edges >>= aux -- note that aux is being called within its own definition (i.e. aux is recursive). The foldM only iterates on the neighbours of v, it does not execute the while loop itself in Dijkstra's
  aux (S.singleton (0, src))
  m <- freeze min_distance
  p <- freeze previous
  return (m, p)
  where
    b = bounds adj_list
    newSTArray :: (Ix i) => (i, i) -> e -> ST s (STArray s i e)
    newSTArray = newArray

shortestPathTo :: (Ix v) => v -> v -> Array v v -> [v]
shortestPathTo target invalid_index previous =
  aux target []
  where
    aux vertex acc
      | vertex == invalid_index = acc
      | otherwise = aux (previous ! vertex) (vertex : acc)

adjList :: Array Char [(Char, Int)]
adjList =
  listArray
    ('a', 'f')
    [ [('b', 7), ('c', 9), ('f', 14)], -- a
      [('a', 7), ('c', 10), ('d', 15)], -- b
      [('a', 9), ('b', 10), ('d', 11), ('f', 2)], -- c
      [('b', 15), ('c', 11), ('e', 6)], -- d
      [('d', 6), ('f', 9)], -- e
      [('a', 14), ('c', 2), ('e', 9)] -- f
    ]

main :: IO ()
main = do
  let (min_distance, previous) = dijkstra 'a' ' ' adjList
  putStrLn $ "Distance from a to e: " ++ show (min_distance ! 'e')
  let path = shortestPathTo 'e' ' ' previous
  putStrLn $ "Path: " ++ show path

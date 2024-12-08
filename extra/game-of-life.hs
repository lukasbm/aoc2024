import Data.Set qualified as Set

neighbors (x, y) = [(x', y') | x' <- [x - 1 .. x + 1], y' <- [y - 1 .. y + 1]]

live u c = length (filter (`Set.member` u) (neighbors c)) `elem` survive
  where
    survive = if c `Set.member` u then [3, 4] else [3]

generation u = Set.filter (live u) checkCells
  where
    checkCells = Set.fromList . concatMap neighbors . Set.toList $ u



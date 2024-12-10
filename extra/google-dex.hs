
pairwiseL1 :: n=>d=>Real -> n=>n=>Real
pairwiseL1 x = for i j.
  sum (for k. abs (x.i.k - x.j.k))

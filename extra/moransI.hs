import Data.List (genericLength)

-- Calculate Moran's I
moransI :: [Double] -> [[Double]] -> Double
moransI values weights =
  let n = genericLength values -- Number of spatial units
      xBar = mean values -- Mean of the values
      deviations = map (\x -> x - xBar) values -- Deviations from the mean
      wSum = sum (concat weights) -- Total sum of weights

      -- Numerator: sum of weighted cross-products of deviations
      numerator =
        sum
          [ weights !! i !! j * deviations !! i * deviations !! j
            | i <- [0 .. length values - 1],
              j <- [0 .. length values - 1]
          ]

      -- Denominator: sum of squared deviations
      denominator = sum [dev ^ 2 | dev <- deviations]
   in (n / wSum) * (numerator / denominator)

-- Helper function to calculate mean
mean :: [Double] -> Double
mean xs = sum xs / genericLength xs

main :: IO ()
main = do
  -- Example values
  let values = [1.0, 2.0, 3.0, 4.0] -- Observed values at spatial units

  -- Example weights (spatial relationship between units)
  let weights =
        [ [0, 0, 0, 0], -- Unit 1
          [1, 0, 1, 1], -- Unit 2
          [1, 1, 1, 1], -- Unit 3
          [0, 1, 1, 0] -- Unit 4
        ]

  -- Calculate Moran's I
  let result = moransI values weights

  -- Output the result
  print result

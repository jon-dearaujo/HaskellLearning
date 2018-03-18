module SumSquares10NaturalNumbers where
  square :: Int -> Int
  square x = x * x 

  sumOfSquares :: Int -> Int -> Int
  sumOfSquares remainingCount total =
    if remainingCount == 0
      then total
      else sumOfSquares (remainingCount - 1) (total + square remainingCount)

  squareOfSum :: Int -> Int -> Int
  squareOfSum remainingCount total =
    if remainingCount == 0
      then square total
      else squareOfSum (remainingCount - 1) (total + remainingCount)

  difference :: Int -> Int
  difference n = squareOfSum n 0 - sumOfSquares n 0
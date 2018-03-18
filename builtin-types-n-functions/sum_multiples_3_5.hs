module SumMultiplesOf3And5 where

  isMultipleOf3Or5 :: Int -> Bool
  isMultipleOf3Or5 x = mod x 3 == 0 || mod x 5 == 0

  sumMultiplesOf3And5UpTo :: Int -> Int -> Int
  sumMultiplesOf3And5UpTo currentNumber total =
    if currentNumber == 0
      then total
      else if isMultipleOf3Or5 (currentNumber - 1)
        then sumMultiplesOf3And5UpTo (currentNumber - 1) (total + currentNumber - 1)
        else sumMultiplesOf3And5UpTo (currentNumber - 1) total

module SumOfXMultiplesOf3And5 where

  sumOfXMultiplesOf3And5 :: Int -> Int -> Int
  sumOfXMultiplesOf3And5 remainingCount total = sumX3Or5Multiples_ 1 remainingCount total

  isMultipleOf3Or5_ :: Int -> Bool
  isMultipleOf3Or5_ x = mod x 3 == 0 || mod x 5 == 0

  sumX3Or5Multiples_ :: Int -> Int -> Int -> Int
  sumX3Or5Multiples_ cn rc t = 
    if rc == 0
      then t
      else if isMultipleOf3Or5_ cn
        then sumX3Or5Multiples_ (cn + 1) (rc - 1) (t + cn)
        else sumX3Or5Multiples_ (cn + 1) rc t
module SumEvenFibonacci where
  fibonacci :: Int -> Int
  fibonacci n =
    if n == 1 || n == 0
      then 1
      else fibonacci (n - 1) + fibonacci (n - 2)

  evenSum :: Int -> Int -> Int -> Int
  evenSum maxValue n total =
    if fibonacci n >= maxValue
      then total
      else if mod (fibonacci n) 2 == 0
        then evenSum maxValue (n + 1) (total + fibonacci n)
        else evenSum maxValue (n + 1) total
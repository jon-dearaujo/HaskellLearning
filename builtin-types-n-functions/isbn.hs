module Isbn where
  import Data.Char

  isValidIsbn :: String -> Bool
  isValidIsbn isbn =
    validateIsbn isbn 10 0
  
  validateIsbn :: String -> Int -> Int -> Bool
  validateIsbn remainingIsbn currentMultiplier total =
    if currentMultiplier == 1
      then mod (digitToInt (head remainingIsbn) + total) 11 == 0
      else validateIsbn (tail remainingIsbn) (currentMultiplier - 1) (total + (digitToInt (head remainingIsbn) * currentMultiplier))
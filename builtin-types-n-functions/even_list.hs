module EvenList where

  evenList :: Int -> [Int] -> [Int]
  evenList remaining list = 
    if remaining == 0
      then list
      else if (length list) == 0
        then evenList (remaining - 1) [remaining * 2]
        else evenList (remaining - 1) (((head list) - 2):list)

arrow :: Integer -> Integer -> Integer
arrow k n = if n == 1 -- Rekursionsanker
  then k
  else k ^ arrow k (n - 1)

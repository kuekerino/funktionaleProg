arrow :: Double -> Double -> Double
arrow k n = if n == 1
  then k 
  else k ** arrow k (n - 1)

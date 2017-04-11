fac :: Int -> Int
fac n | n == 0    = 1
      | otherwise = n * fac (n - 1)
      
fac' :: Int -> Int
fac' n = if n == 0 then 1 else n * fac' (n-1)

fac'' :: Int -> Int
fac'' 0 = 1
fac'' n = n * fac'' (n-1)

sumFacs :: Int -> Int -> Int
sumFacs n m | n == m = fac m
            | n > m  = 0
            | n < m  = fac n + sumFacs (n+1) m


fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs :: [Int]
fibs = go 0
  where go :: Int -> [Int]
        go i = fib i : go (i+1)
        
foo :: [Int] -> Int
foo xs = 42

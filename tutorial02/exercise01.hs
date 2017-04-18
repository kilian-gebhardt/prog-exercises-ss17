pack :: [Char] -> [[Char]]
pack []     = []
pack (x:xs) = help [] [x] xs 

help :: [[Char]] -> [Char] -> [Char] -> [[Char]]
help output current [] = output ++ [current]
help output (x:current') (y:inp') 
  | x == y = help output (y:x:current') inp'
  | x /= y = help (output ++ [ x:current']) [y] inp'
  
encode :: [Char] -> [(Int, Char)]
encode xs = encode' (pack xs)

encode' :: [[Char]] -> [(Int, Char)]
encode' [] = []
encode' (xs:ys) = (length xs, head xs) : encode' ys

times :: Int -> Char -> [Char]
times 0 _ = []
times x c = c : times (x-1) c

decode :: [(Int, Char)] -> [Char]
decode [] = []
decode ((x, c):ys) = times x c ++ decode ys

rotate :: [Int] -> Int -> [Int]
rotate [] _ = []
rotate xs n = rotate' xs (n `mod` (length xs))

rotate' :: [Int] -> Int -> [Int]
rotate' xs 0 = xs
rotate' (x:xs) n = rotate' (xs ++ [x]) (n-1)

























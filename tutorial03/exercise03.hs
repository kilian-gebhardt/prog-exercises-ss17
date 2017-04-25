-- 

even' :: Int -> Bool
even' x = if x `mod` 2 == 0 then True else False

evens :: [Int] -> [Int]
evens xs = filter even' xs

square :: Int -> Int
square x = x * x
-- square = (\x -> x * x)
--          (\x -> x^2)
--          (^2)

squares :: [Int] -> [Int]
squares xs = map square xs

prod :: [Int] -> Int
prod xs = foldr (*) 1 xs

productOfSquaresOfEvens :: [Int] -> Int
productOfSquaresOfEvens xs = prod (squares (evens xs))

posof = foldr (*) 1 . map (^2) . filter even

-- ich bin ein Kommentar

prod :: [Int] -> Int
prod []  = 1 -- da neutrales Element der Multiplikation 1 ist
prod [x] = x -- optional
prod (x:xs) = x * prod xs

rev :: [Int] -> [Int]
rev [] = []
rev [x]    = [x] -- optional
rev (x:xs) = rev xs ++ [x]
 
append :: [Int] -> [Int] -> [Int]
append [] ys = ys
--append []     []     = []
--append []     (y:ys) = y:ys
append (x:xs) ys = x : append xs ys
-- append (x:xs) []     = x:xs
-- append (x:xs) (y:ys) = x : append xs (y:ys)

rev' :: [Int] -> [Int]
rev' xs = go xs []
    where go :: [Int] -> [Int] -> [Int]
          go []     rs = rs
          go (x:xs) rs = go xs (x:rs)
          
isOrd :: [Int] -> Bool -- elements of Bool: True & False
--isOrd (x:y:ys) | x < y     = isOrd (y:ys)
--               | otherwise = False
isOrd (x:y:ys) = (x < y) && isOrd (y:ys)
isOrd xs = True


merge :: [Int] -> [Int] -> [Int]
merge []     []     = []
merge (x:xs) []     = x:xs
merge []     (y:ys) = y:ys
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys 

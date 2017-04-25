data Tree a = Branch a (Tree a) (Tree a) | Leaf a deriving (Show)

t = Branch 3 (Leaf 4) (Branch 7 (Leaf 5) (Leaf 2))

depth :: Tree a -> Int
depth (Leaf _) = 1
depth (Branch _ l r) = 1 + min (depth l) (depth r)


paths :: Tree a -> Tree [a]
paths = paths' [] 
-- paths t = paths' [] t

paths' :: [a] -> Tree a -> Tree [a]
paths' pre (Leaf x) = Leaf (pre ++ [x])
paths' pre (Branch x l r) 
  = let xs = pre ++ [x]
    in Branch xs (paths' xs l) (paths' xs r)

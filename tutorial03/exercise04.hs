data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Show)

t = Node 3 (Leaf 4) (Node 7 (Leaf 5) (Leaf 2))

tmap :: (a -> b) -> Tree a -> Tree b
tmap f (Leaf x) = Leaf (f x)
tmap f (Node x l r) = Node (f x) (tmap f l) (tmap f r)

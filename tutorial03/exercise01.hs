data Tree = Leaf Int | Branch Tree Tree deriving (Show)

l1 = Leaf 4
l2 = Leaf 3
l3 = Leaf 7

t1 = Branch l1 l2
t2 = Branch l3 t1
t  = Branch t1 t2

countLeaves :: Tree -> Int
countLeaves (Leaf x) = 1
countLeaves (Branch l r) = countLeaves l + countLeaves r

leaves :: Tree -> [Int]
leaves (Leaf x) = [x]
leaves (Branch l r) = leaves l ++ leaves r

--1
data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero n = n
mult (Succ m) (Succ n) = add (mult (Succ m) n) (Succ n)

--2
data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 
         (Node (Leaf 6) 7 (Leaf 9))

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) | compare x y == EQ = True
                      | compare x y == LT = occurs x l
                      | compare x y == GT = occurs x r
-- Same explanation as on page 98, only look in correct side of the tree

--3
data Treeb a = Leafb a | Nodeb (Treeb a) (Treeb a)

balanced :: Treeb a -> Bool
balanced (Nodeb (Nodeb (Nodeb _ _) (Nodeb _ _)) (Leafb _)) = False
balanced (Nodeb (Leafb _) (Nodeb (Nodeb _ _) (Nodeb _ _))) = False
balanced (Nodeb l r) = balanced l && balanced r
balanced (Leafb _) = True 

tb :: Treeb Int
tb = Nodeb (Leafb 1) (Nodeb (Nodeb (Leafb 2) (Leafb 3)) (Nodeb (Leafb 4) (Leafb 5)))

tc :: Treeb Int
tc = Nodeb (Leafb 1) (Leafb 5)

--4
halve :: [a] -> ([a], [a])
halve x = (take l x, drop l x)
        where l = length x `div` 2

balance :: [a] -> Treeb a
balance (x:[]) = Leafb x
balance x = Nodeb (balance l) (balance r)
  where (l, r) = halve x
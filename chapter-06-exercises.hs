--1
fact :: Int -> Int
fact n | n == 0 = 1
       | n < 0 = -1
       | otherwise = n * fact(n-1)

--2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

--3
(^^^) :: Int -> Int -> Int
(^^^) 0 _ = 0
(^^^) m 1 = m
(^^^) m n = m*m^^^(n-1)

--4
euclid :: Int -> Int -> Int
euclid m n | m == 0 = 0
           | n == 0 = 0
           | y `mod` x == 0 = x
           | otherwise = euclid y (x-1)
           where 
               x = min m n
               y = max m n

--6
--a
andd :: [Bool] -> Bool
andd [] = True
andd (x:xs) = x && andd xs 

--b
conc :: [[a]] -> [a]
conc [] = []
conc (x:xs) = x ++ conc xs

--c
repl :: Int -> a -> [a]
repl 0 _ = []
repl n x = [x] ++ repl (n-1) x

--d 
(!!!) :: [a] -> Int -> a
(!!!) (x:_) 0 = x
(!!!) (_:xs) n = xs !!! (n-1)

--e
eleme :: Eq a => a -> [a] -> Bool
eleme _ [] = False
eleme y (x:xs) = y == x || eleme y xs

--7
merge :: Ord a => [a] -> [a] -> [a]
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys) = insert y (insert x (merge xs ys))

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x:y:ys
                | otherwise = y:insert x ys

--8
halve :: [a] -> ([a], [a])
halve x = (take l x, drop l x)
        where l = length x `div` 2

msort :: Ord a => [a] -> [a]
msort [x] = [x]
msort x = merge (msort m) (msort n)
  where
    (m, n) = halve x
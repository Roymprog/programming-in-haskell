import Data.Char

--1
-- sum [x | x <- [1..100]]

--2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

--3
square :: Int -> [(Int, Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

--4
repl :: Int -> a -> [a]
repl n x = [x | _ <- [1..n]] 

--5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n],x^2 + y^2 == z^2]

--6
divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], n `mod` x == 0, x /= n]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum(divisors x) == x]

--9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct m n = sum [ x*y | (x,y) <- zip m n]

--10
let2int :: Char -> Int
let2int c | ord c < 97 = ord c - ord 'a' + 58 
          | otherwise = ord c - ord 'a'

int2let :: Int -> Char
int2let i | i < 26 = chr(ord 'a' + i)
          | otherwise = chr(ord 'A' + i - 26)

shift :: Int -> Char -> Char
shift i c | isLetter c = int2let ((let2int c + i) `mod` 52)
          | otherwise = c

encode :: Int -> String -> String
encode i s = [shift i x | x <- s]



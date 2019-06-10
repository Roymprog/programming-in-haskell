--1
-- map f . filter p x

--2
--a
-- all :: (a -> Bool) -> [Bool] -> Bool
all p = and . map p

--b
-- any :: (a -> Bool) -> [Bool] -> Bool
any p = or . map p

--c
-- takeWhiles :: (a -> Bool) -> [a] -> [a]
takeWhiles _ [] = []
takeWhiles p (x:xs) | p x = x : takeWhile p xs
                    | otherwise = []

--d
-- dropWhiles :: (a -> Bool) -> [a] -> [a]
dropWhiles _ [] = []
dropWhiles p (x:xs) | p x = dropWhiles p xs
                    | otherwise = x:xs

--3
mapf :: (a -> b) -> [a] -> [b]
mapf f = foldr (\x acc -> acc++[f x]) []

filterf :: (a -> Bool) -> [a] -> [a]
filterf f = foldr (\x acc -> if f x then [x] ++ acc else acc) []

--4
dec2int :: [Int] -> Int
dec2int decs = foldl (\sum (x,w) -> sum+x*w) 0 tuple
         where weights = iterate (10*) 1
               tuple = zip (reverse decs) weights

--5
curr :: ((a, b) -> c) -> (a -> b -> c)
curr f = \x -> \y -> f (x, y)

uncurr :: (a -> b -> c) -> ((a, b) -> c)
uncurr f (x, y) = f x y

--6

unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

type Bit = Int
chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (== []) (take 8) (drop 8)

-- mapp :: (a -> b) -> [a] -> [b]
-- mapp f = unfold (== []) (app_head) (tail)
--   where app_head = f $ head
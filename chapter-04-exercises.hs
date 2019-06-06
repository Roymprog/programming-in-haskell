-- 1

halve :: [a] -> ([a], [a])
halve x = (take l x, drop l x)
        where l = length x `div` 2

-- 2

thirda :: [a] -> a
thirda x = head $ tail $ tail x 

thirdb :: [a] -> a
thirdb x = x !! 2

thirdc :: [a] -> a
thirdc (_:_:x:_) = x

--3

safetaila :: [a] -> [a]
safetaila x = if null x then x else tail x

safetailb :: [a] -> [a]
safetailb x | null x = x
            | otherwise = tail x

safetailc :: [a] -> [a]
safetailc (_:xs) = xs
safetailc x = x

--4

--(|||) :: Bool -> Bool -> Bool
--True ||| _ = True
--__ ||| True = True
--False ||| False = False

(|||) :: Bool -> Bool -> Bool
True ||| _ = True
False ||| a = a

--5

(&&&) :: Bool -> Bool -> Bool
(&&&) x y = if x then if y then True else False else False

--6 

(&&&&) x y = if x then y else False

--7 

multl :: Int -> (Int -> (Int -> Int))
multl = \x -> (\y -> (\z -> x*y*z))

--8

luhnDouble :: Int -> Int
luhnDouble x | 2*x > 9 = 2*x-9
             | otherwise = 2*x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn k l m n = sum [luhnDouble k,l,luhnDouble m,n] `mod` 10 == 0

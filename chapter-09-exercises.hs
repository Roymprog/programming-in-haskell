subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
          where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat ( map (interleave x) (perms xs))

-- Get all permutations on all subsets of x
choices :: [a] -> [[a]]
choices x = [ys | zs <- subs x, ys <- perms zs]

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] [] = True
isChoice [] _ = True
isChoice (x:xs) y = elem x y && isChoice xs ys
          where ys = dropFirst x y

dropFirst :: Eq a => a -> [a] -> [a]
dropFirst _ [] = []
dropFirst x (y:ys) | x == y = ys
                   | otherwise = y : dropFirst x ys

-- Split will no longer reduce the length of the lists, a list with equal
-- length will be produced that will be split to a list of equal lenght
-- on a consecutive call to exprs. So, exprs will loop forever.  
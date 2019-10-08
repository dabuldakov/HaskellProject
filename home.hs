import Data.List

------------------Combinations

combinations :: (Eq a, Ord a) => Int -> [a] -> [[a]]
combinations 1 xs = sort $ map (\x-> [x]) xs
combinations n xs = sort $ nub $ map (sort. take n)  (permutations xs) 

{-combinations' :: (Eq a, Ord a) => Int -> [a] -> [[a]]
combinations' n l | (length l) == n = []
                  | otherwise       = check (take (n-1) l) (drop (n-1) l) (drop (n-1) l)
 where
  check a  [] last   |  combinations' n (tail l)
  check a  [] last   = check (take (n-2) l) (drop (n-1) last
  check a (b:bs) last = [a ++ [b]] ++ (check a bs last)-}
{-
combinations' :: (Eq a, Ord a) => Int -> [a] -> [[a]]
combinations' n l = check (take (n-1) l) (drop (n-1) l) (drop (n-1) l) 1
 where
  check a []     []  k = combinations' n (tail l)
  check a []     las k = check ((take (n-(k+1)) a) ++ [head las]) (tail las) (tail las) k
  check a (b:bs) las k = [a ++ [b]] ++ (check a bs las k) -}

combinations' :: (Eq a, Ord a) => Int -> [a] -> [[a]]
combinations' n l = check (take (n-1) l) (drop (n-1) l) (drop (n-1) l) 1
 where
  check a []     []  k | k == (n-2) = combinations' n (tail l)
  check a []     []  k = let las = drop (k+n) l in check ((take (n-(k+2)) a) ++ (take (k+1) las)) las las (k+1)
  check a []     las k = check ((take (n-(k+1)) a) ++ (take k las)) (drop k las) (drop k las) k
  check a (b:bs) las k = [a ++ [b]] ++ (check a bs las k)



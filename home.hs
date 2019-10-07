import Data.List

------------------Combinations

combinations :: (Eq a, Ord a) => Int -> [a] -> [[a]]
combinations 1 xs = sort $ map (\x-> [x]) xs
combinations n xs = sort $ nub $ map (sort. take n)  (permutations xs) 

combinations' :: (Eq a, Ord a) => Int -> [a] -> [[a]]
combinations' n l | (length l) == n = []
                  | otherwise       = check (take (n-1) l) (drop (n-1) l) (drop (n-1) l)
 where
  check a  [] last   |  combinations' n (tail l)
  check a  [] last   = check (take (n-2) l) (drop (n-1) last
  check a (b:bs) last = [a ++ [b]] ++ (check a bs last)

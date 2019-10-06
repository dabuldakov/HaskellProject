import Data.List

------------------Combinations

combinations :: (Eq a, Ord a) => Int -> [a] -> [[a]]
combinations 1 xs = sort $ map (\x-> [x]) xs
combinations n xs = sort $ nub $ map (sort. take n)  (permutations xs) 
import Data.List

------------------Combinations

combinations :: (Eq a, Ord a) => Int -> [a] -> [[a]]
combinations 1 xs = sort $ map (\x-> [x]) xs
combinations n xs = sort $ nub $ map (sort. take n)  (permutations xs) 

combinations' :: (Eq a, Ord a) => Int -> [a] -> [[a]]
combinations' n l | (length l) == n = []
                  | otherwise       = check (take (n-1) l) (drop (n-1) l) (drop (n-1) l) 1
 where
  check a  [] last  k | (n-k) == 1 =  combinations' n (tail l)
  check a  bs  last k |  ==                    | 
  check a  [] last  k = check (take (n-k) ++ [(head last) l)] (init last) (init last) k
  check a (b:bs) last k = [a ++ [b]] ++ (check a bs last k)

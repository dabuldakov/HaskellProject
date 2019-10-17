import Data.List

------------------Combinations

combinations :: (Eq a, Ord a) => Int -> [a] -> [[a]]
combinations 1 xs = sort $ map (\x-> [x]) xs
combinations n xs = sort $ nub $ map (sort. take n)  (permutations xs) 

{-combinations' :: (Eq a, Ord a) => Int -> [a] -> [[a]]
combinations' n l | (length l) == n = []
                  | otherwise       = check (take (n-1) l) (drop (n-1) l) (drop (n-1) l) 1
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
combinations' n l = check (take (n-1) l) (drop (n-1) l) (drop (n-1) l) (n-2)
 where
  check a []     []  k | k == (n-2) = combinations' n (tail l)
  check a []     []  k = let las = drop (k+n) l in check (init a ++ (take (k+1) las)) las las (k+1)
  check a []     las k = check ((init a) ++ [head las]) (tail las)  (tail las) k
  check a (b:bs) las k = [a ++ [b]] ++ (check a bs las k)

combinations'' :: (Eq a, Ord a) => Int -> [a] -> [[a]]
combinations'' n xs = sort $ map sort $ map (map (snd)) $ nub $ map (sort. take n)  (permutations (zip [1..] xs))


combinatTest x = combinat x []
 where 
  combinat 0 p = putStrLn $ show p
  combinat n p = do 
      combinat (n-1) (p ++ ['a'])
      combinat (n-1) (p ++ ['o'])
      combinat (n-1) (p ++ ['y'])


---------------------

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => (a,b) -> Tree (a,b) -> Tree (a,b)
treeInsert x EmptyTree = singleton x
treeInsert (x,y) (Node (a,b) left right)
 | x == a = Node (x,y) left right
 | x < a = Node (a,b) (treeInsert (x,y) left) right
 | x > a = Node (a,b) left (treeInsert (x,y) right)

treeElem :: (Ord a) => a -> Tree (a,b) -> Maybe (a,b)
treeElem x EmptyTree = Nothing
treeElem x (Node (a,b) left right)
 | x == a = Just (a,b)
 | x < a = treeElem x left
 | x > a = treeElem x right


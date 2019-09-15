import Data.Char
import Data.List

doubleMe' x = x + x 
length' xs = sum [1 | _ <- xs]

removeNonUppercase :: [Char] -> [Char]  
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer  
factorial n = product [1..n] 

circumference :: Float -> Float  
circumference r = 2 * pi * r

circumference' :: Double -> Double  
circumference' r = 2 * pi * r

factorial5 n | n >=0 = helper 1 n
factorial5 n | otherwise = error "abs shoud be > 0"

helper acc 0 = acc
helper acc n = helper (acc*n) (n-1)

doubleUs x y = doubleMe x + doubleMe y
doubleMe x = x + x 
doubleSmallNumber x = (if x > 100 then x else x*2) + 1
conanO'Brien = "It's a-me, Conan O'Brien!"
boomBangs xs = [if x > 10 then "BIG" else "LOW" | x <- xs, odd x]

lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky 777 = "LUCKY 777"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5" 

factorial' :: (Integral a) => a -> a  
factorial' 0 = 1  
factorial' n = n * factorial' (n - 1) 

charName :: Char -> String  
charName 'a' = "Albert"  
charName 'b' = "Broseph"  
charName 'c' = "Cecil"
charName  x  = "Another"

length'' xs = sum[1 | _ <- xs]

-----------------

data Shape = Circle Point Float | Rectangle Point Point 
 deriving (Show)
area :: Shape -> Float
area (Circle _ r) = pi * r^2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

data Point = Point Float Float 
 deriving (Show)
 
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))
 
baseCircle :: Float -> Shape 
baseCircle r = Circle (Point 0 0) r 

baseRect :: Float -> Float -> Shape
baseRect width heith = Rectangle (Point 0 0) (Point width heith)

-----------------

test0709'1 (x:y:xs) = show x ++ " -- " ++ show y

------------------

test0709'2 line@(x:xs) = "Length: " ++ show (length line) ++ ". First symbol: " ++ [x]

----------------

bmiTell :: Double -> Double -> String 
bmiTell weight height   
  | x <= skiny = "distof"   
  | x <= normal = "norm"   
  | x <= fat = "fat"   
  | otherwise = "very fat"
  where 
  x = weight / height ^ 2
  (skiny, normal, fat) = (18.5, 25.0, 30.0)

---------

initials :: String -> String -> String 
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
 where 
 (f:_) = firstname 
 (l:_) = lastname
 
------------
 
calcBmis :: [(Double, Double)] -> [Double] 
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2] 

---------------

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

--------

replicate' :: Int -> a -> [a]
replicate' x y 
  | x <= 0 = []
  | otherwise = y : (replicate' (x-1) y)
  
take' n list
  | n <=0 = []
  | list == [] = []
take' n (x:xs) = x : (take' (n-1) xs)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x: repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : (zip' xs ys)

elem' _ [] = False
elem' n (x:xs)
   | n == x = True
   | otherwise = elem' n xs

------------

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort a ++ [x] ++ quickSort b 
 where
  a = filter (\q -> q <= x) xs 
  b = filter (\q -> q > x) xs
  
quickSort' :: (Ord a) => [a] -> [a]
quickSort' [] = []
quickSort' (x:xs) = quickSort a ++ [x] ++ quickSort b 
 where
  a = [y | y <- xs, y <= x]
  b = [y | y <- xs, y >  x]

quickSort'' :: (Ord a) => [a] -> [a]
quickSort'' [] = []
quickSort'' (x:xs) = 
 let 
  a = quickSort'' (filter (<= x) xs) 
  b = quickSort'' (filter (> x) xs)
 in
  a ++ [x] ++ b 


  
---------------------
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

------------------------

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = q 
 where q x y = f y x 

--------------------------

ld = head (filter p [100000,99999..])
 where p a = a `mod` 3829 == 0

ss = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

ss' = sum (takeWhile (<10000) [m | m <- [n^2 | n <- [1..]], odd m])


chain :: Integer -> [Integer]
chain 1 = [1]
chain x | odd x = x:chain (x * 3 + 1)
        | even x = x:chain (x `div` 2)

numLC :: Integer
numLC = fromIntegral (length (filter isLong (map chain [1..100])))
 where isLong x = length x > 15

numLC' :: Integer
numLC' = fromIntegral (length (filter (\a -> length a > 15) (map chain [1..100])))

---------------
---------------------------------Remove duplicate words

removeDuplicateWordsMy :: String -> String
removeDuplicateWordsMy a = unwords (check (words a) [])
 where check   []   y = y 
       check (x:xs) y = if (filter (== x) y) == [] then check xs (y ++ [x]) else check xs y 

removeDuplicateWords :: String -> String
removeDuplicateWords = unwords . reverse . check . reverse . words
 where check   []   = [] 
       check (x:xs) = if x `elem` xs then check xs else x : check xs 

removeDuplicateWords' :: String -> String 
removeDuplicateWords' = unwords. nub. words 

removeDuplicateWords'' :: String -> String
removeDuplicateWords'' = unwords . foldl (\acc w -> if w `notElem` acc then acc ++ [w] else acc) [] . words

removeDuplicateWords''' :: String -> String
removeDuplicateWords''' = unwords . foldl (\a x -> if elem x a then a else a ++ [x]) [] . words

----intercalate " " ["asd", "asd", "a", "sss"] разбивает по нужному имволу 
-----"asd asd a sss"

----------------Sum of odd numbers

rowSumOddNumbers :: Integer -> Integer
rowSumOddNumbers x = sum (take (fromIntegral x) (reverse (take (fromIntegral (sum [1..x])) [1,3..])))

rowSumOddNumbers' :: Integer -> Integer
rowSumOddNumbers' x = sum list
 where 
  i = fromIntegral (sum [1..x])
  xs = take i [1,3..] 
  list = drop (i - (fromIntegral x)) xs 
  
rowSumOddNumbers'' :: Integer -> Integer -------------------самый быстрый
rowSumOddNumbers'' x = sum list
 where
  list = take (fromIntegral x) [first, (first + 2)..] 
  first = sum (take (fromIntegral (x-1)) [2,4..]) + 1
  
rowSumOddNumbers''' :: Integer -> Integer -------------------самый быстрый
rowSumOddNumbers''' x = sum list
 where
  list = [first, (first + 2)..(first + (last k))] 
  first = sum (k) + 1
  k = take (fromIntegral (x-1)) [2,4..]
  
rowSumOddNumbers'''' :: Integer -> Integer
rowSumOddNumbers'''' x = x^3
 
---------------------Product Of Maximums Of Array (Array Series #2) 

maxProduct :: [Integer] -> Int -> Integer
maxProduct list x = product (take x (reverse (quickSort list)))

maxProduct' :: [Integer] -> Int -> Integer
maxProduct' list x = product (take x (reverse (sort list)))

-------------------Minimize Sum Of Array (Array Series #1) 

minSum :: [Integer] -> Integer
minSum = sum . check . reverse . sort
 where 
  check [] = []
  check (x:xs) = x * last xs : (check $ init xs)

-------------------Growth of a Population

nbYear :: Int -> Double -> Int -> Int -> Int
nbYear p0 percent aug p = check (fromIntegral p0) 0
 where 
  check :: Double -> Int -> Int
  check acc n | acc < (fromIntegral p) = check (fromIntegral (truncate (acc * (1 + percent / 100))) + (fromIntegral aug)) (n + 1)
              | otherwise = n

-----------------Array Leaders (Array Series #3)

arrayLeaders :: [Integer] -> [Integer]
arrayLeaders [] = []
arrayLeaders (x:xs) | x > sum xs = x : arrayLeaders xs
                    |  otherwise = arrayLeaders xs
    
----------------Maximum Gap (Array Series #4)

maxGap :: [Int] -> Int
maxGap = check . reverse . sort
 where 
   check   (y:[]) = 0
   check (x:y:xs) = max (x - y) (check $ y:xs)

maxGap' :: [Int] -> Int
maxGap' = maximum . diffs . sort
 where 
   diffs :: [Int] -> [Int]
   diffs xs = zipWith (-) (tail xs) (init xs)
   
maxGap'' :: [Int] -> Int
maxGap'' xs = maximum $ zipWith (-) (tail $ sort xs) (sort xs)

-----------------Product Array (Array Series #5)

productArray :: [Integer] -> [Integer]
productArray x = map (div p) x
 where p = product x

------------------Maximum Triplet Sum (Array Series #7) 

maxTriSum :: [Integer] -> Integer
maxTriSum = sum . take 3 . reverse . check . sort
 where
  check [] = []
  check (x:xs) = if (x `elem` xs) then check xs else x : check xs  

maxTriSum' :: [Integer] -> Integer
maxTriSum' = sum . take 3 . reverse . sort . nub    ------nub убираем дубли







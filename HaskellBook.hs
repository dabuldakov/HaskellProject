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
  
---------------------
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = q 
 where q x y = f y x 
























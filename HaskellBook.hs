import Data.Char
import Data.List
import qualified Data.Map as Map

import qualified Geometry.Sphere as Sphere 
import qualified Geometry.Cuboid as Cuboid 
import qualified Geometry.Cube as Cube

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


  
---------------------5 Функции высокого порядка

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

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' y ys = foldl (\acc x -> if y == x then True else acc) False ys

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

-------------------6. Модули

numWords :: String -> [(String, Int)]
numWords = map (\x -> (head x, length x)) . group . sort . words

----------------------

isIn :: (Eq a) => [a] -> [a] -> Bool
isIn x y = any (isPrefixOf x) $ tails y
---------------------

encode :: Int -> String -> String
encode offset s = map (\a -> chr $ ord a + offset) s

encode' :: Int -> String -> String
encode' offset s = map (chr . (+offset) . ord) s

decode :: Int -> String -> String
decode shift = encode $ negate shift
-----------------------

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo40 :: Int -> Maybe Int
firstTo40 a = find (\x -> (digitSum x) == a) [1..]
-------------------------

findKey :: (Eq k) => k -> [(k,v)] -> v 
findKey key xs = snd . head $ filter (\(k,v) -> key == k) xs 

findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key [] = Nothing
findKey' key ((k,v):xs) | key == k = Just v
                        | otherwise = findKey' key xs

findKey'' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey'' key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing
--------------------------------

phoneBook :: Map.Map String String
phoneBook = Map.fromList $ 
 [("a","555-29-38")
 ,("s","452-29-28")
 ,("d","493-29-28")
 ,("f","205-29-28")
 ,("f","222-29-28")
 ,("gg","939-82-82")
 ,("gg","853-24-92")]
 
phoneBook' = 
 [("a","555-29-38")
 ,("s","452-29-28")
 ,("d","493-29-28")
 ,("f","205-29-28")
 ,("f","222-29-28")
 ,("gg","939-82-82")
 ,("gg","853-24-92")]

string2Digits :: String -> [Int]
string2Digits = map digitToInt . filter isDigit

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith add xs
 where add number1 number2 = number1 ++ ", " ++ number2

phoneBookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a] 
phoneBookToMap' xs = Map.fromListWith (++) $ map (\(k,v) -> (k, [v])) xs

---------------7. СОЗДАНИЕ НОВЫХ ТИПОВ И КЛАССОВ ТИПОВ


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
--------------------Valid Braces

validBraces :: String -> Bool
validBraces s = if length (delScobe (div (length s) 2) s) > 0 then False else True
 where
  delScobe 0 acc = acc
  delScobe n acc = delScobe (n-1) (check acc)
  check [] = []
  check (x:[]) = x:[]
  check (x:y:xs) | x == '[' && y == ']' || x == '(' && y == ')' || x == '{' && y == '}' = check xs
                 | otherwise = x : (check $ y:xs) 

validBraces' :: String -> Bool
validBraces' s = null (delScobe (div (length s) 2) s)
 where
  pairs = ["[]", "()", "{}"]
  delScobe 0 acc = acc
  delScobe n acc = delScobe (n-1) (check acc)
  check [] = []
  check (x:[]) = x:[]
  check (x:y:xs) | elem (x:y:[]) pairs  = check xs
                 | otherwise = x : (check $ y:xs) 

validBraces'' :: String -> Bool
validBraces'' ""     = True
validBraces'' (x:xs) = go [x] xs
  where go ('(':xs) (')':ys) = go xs ys
        go ('[':xs) (']':ys) = go xs ys
        go ('{':xs) ('}':ys) = go xs ys
        go xs       (y:ys)   = go (y:xs) ys
        go []       []       = True
        go _        []       = False

--------------------Product of consecutive Fib numbers

productFib :: Integer -> (Integer, Integer, Bool)
productFib n = check (fibonacci 1) (fibonacci 2) 2
 where 
  check i1 i2 r | (i1 * i2) == n = (i1,i2,True)
                | (i1 * i2)  < n = check i2 (fibonacci (r+1)) (r+1)
                | otherwise = (i1,i2,False)
  fibonacci 0 = 0
  fibonacci 1 = 1
  fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

productFib' :: Integer -> (Integer, Integer, Bool)
productFib' n = check 1 1
 where 
  check i1 i2 | i1 * i2 == n = (i1,i2,True)
              | i1 * i2  < n = check i2 (i1+i2)
              | otherwise    = (i1,i2,False)

------------------Reverse words

reverseWords :: String -> String
reverseWords n = check n [] []
 where 
  check     [] word line = line ++ word
  check (x:xs) word line| x /= ' ' = check xs (x : word) line
                        | otherwise = check xs [] (line ++ word ++ [x])

-----------------Snail

snail :: [[Int]] -> [Int]
snail = foldl1 (++) . check 
 where 
  check [] = []
  check (x:xs) = (x : (map (\a -> [last a]) xs)) ++ (check $ reverse $ map (reverse . init) xs)

snail' :: [[Int]] -> [Int]
snail' [] = []
snail' (xs:xss) = xs ++ (snail . reverse . transpose) xss

------------------------Speed Control

gps :: Int -> [Double] -> Int
gps s [] = 0
gps s [x] = 0
gps s x = abs $ round $ minimum $ map ((3600 / (fromIntegral s)) *) $ zipWith (-) x (tail x)

sss :: Int -> Double -> Double
sss s x = (fromIntegral s) / x







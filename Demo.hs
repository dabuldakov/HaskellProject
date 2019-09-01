module Demo where

import Data.Char
import Data.Function

infixl 6 *+*

a *+* b = a ^ 2 + b ^ 2

discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

standardDiscount = discount 1000 5

test x = isDigit x 

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x then if isDigit y then digitToInt x*10 + digitToInt y else 100 else 100

twoDigits2IntV2 :: Char -> Char -> Int
twoDigits2IntV2 x y = if isDigit x && isDigit y then digitToInt x * 10 + digitToInt y else 100

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt $ (fst p2 - fst p1) ^ 2 + (snd p2 - snd p1) ^ 2

factorial n = if n == 0 then 1 else n * factorial (n-1)

factorial' 0 = 1
factorial' n = n * factorial (n-1)

doubleFactorial' 0 = 1
doubleFactorial' 1 = 1
doubleFactorial' n = n * doubleFactorial' (n-2)

doubleFactorial'' 0 = 1
doubleFactorial'' 1 = 1
doubleFactorial'' n = if n < 0 then error "Abs shod be > 0." else n * doubleFactorial'' (n-2)

factorial3 0 = 1
factorial3 n | n < 0 = error "abs shoud be > 0!"
factorial3 n | n > 0 = n * factorial3 (n-1)

factorial4 n | n == 0 = 1
factorial4 n | n > 0 = n * factorial4 (n-1)
factorial4 n | otherwise = error "abs shoud be > 0!"

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

fibonacci' :: Integer -> Integer
fibonacci' 0 = 0
fibonacci' 1 = 1
fibonacci' n | n > 0 = fibonacci' (n - 1) + fibonacci' (n - 2)
fibonacci' n | n < 0 = fibonacci' (n + 2) - fibonacci' (n + 1)

fibonacci'' :: Integer -> Integer
fibonacci'' n | n > 1 = fibonacci'' (n - 1) + fibonacci'' (n - 2)
fibonacci'' n | n < 0 = fibonacci'' (n + 2) - fibonacci'' (n + 1) 
fibonacci'' n | otherwise = n


factorial5 n | n >=0 = helper5 1 n
factorial5 n | otherwise = error "abs shoud be > 0"

helper5 acc 0 = acc
helper5 acc n = helper5 (acc*n) (n-1)

factorial5' n | n >=0 = 
  let 
    helper5' acc 0 = acc
    helper5' acc n = helper5 (acc*n) (n-1)
  in helper5' 1 n
factorial5' n | otherwise = 
  error "abs shoud be > 0"



fibonacci6 :: Integer -> Integer
fibonacci6 n | n >=0 = helper6 0 1 n
fibonacci6 n | otherwise = helper7 0 1 n

helper6 :: Integer -> Integer -> Integer -> Integer
helper6 acc1 acc2 0 = acc1
helper6 acc1 acc2 n = helper6 (acc2) (acc1 + acc2) (n-1)
helper7 :: Integer -> Integer -> Integer -> Integer
helper7 acc1 acc2 0 = acc1
helper7 acc1 acc2 n = helper7 (acc2) (acc1 - acc2) (n+1)

seqA :: Integer -> Integer
seqA n | n <  3 = n
seqA n | n >= 0 = 
 let 
    helperSeqA acc0 acc1 acc2 0 = acc0
    helperSeqA acc0 acc1 acc2 1 = acc1
    helperSeqA acc0 acc1 acc2 2 = acc2
    helperSeqA acc0 acc1 acc2 n = helperSeqA acc1 acc2 (acc2 + acc1 - 2 * acc0) (n-1)
 in helperSeqA 1 2 3 n
seqA n | otherwise = error "abs shoud be >= 0"




roots a b c = 
 (
  (-b - sqrt(b^2 - 4*a*c)) / (2*a)
 ,
  (-b + sqrt(b^2 - 4*a*c)) / (2*a) 
 )

roots' a b c = let {d = sqrt(b^2 - 4*a*c); twice = (2*a)} in
 (
   (-b - d) / twice
  ,  
   (-b + d) / twice
 )

roots'' a b c = 
 let
    d = sqrt(b^2 - 4*a*c)
    twice = 2*a
    x1 = (-b - d) / twice
    x2 = (-b + d) / twice
 in (x1, x2)

rootsDif a b c = 
  let
    (x1, x2) = roots'' a b c
  in x2 - x1

length' xs = sum [1 | _ <- xs]



sum'n'count :: Integer -> (Int, Int)
sum'n'count x | x  > 0 = ((summa 0 (show x) (dlina x - 1)), (dlina x))
sum'n'count x | x  < 0 = sum'n'count (-x)
sum'n'count x | x == 0 = (0, 1)

dlina x = length (show x)
dlina2 x = length (tail (show x))


summa acc x (-1) = acc
summa acc x n = summa (digitToInt(x !! n) + acc) x (n-1)


integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = (funH a b) * ((f a + f b)/2 + funSum f a 0 (funH a b) 0)

  where 
   funH a b = (b - a) / 1000
   funSum f a acc h 1000 = acc
   funSum f a acc h n = funSum f (a + h) (acc + f (a + h)) h (n + 1)   



integration' :: (Double -> Double) -> Double -> Double -> Double
integration' f a b = h * ((f a + f b)/2 + funSum' 0 1)

  where 
   n = 1000
   h = (b - a) / n
   funSum' acc 1000 = acc
   funSum' acc i = funSum' (acc + f (a+h*i)) (i+1) 

getSecondFrom :: q1 -> q2 -> q3 -> q2
getSecondFrom x y z = y

apply2 f x = f (f x)

sumSquares = (+) `on` (^2)

--multSecond = g `on` h

--g = (*)
--h = snd

sumFstFst = (+) `on` helper
 where 
  helper x = fst $ fst x 
  
sumFstFst' = (+) `on` (\x -> fst $ fst x) 

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = (op) (f x) (f y) (f z)

doItYourself = f . g . h

f = logBase 2
g = (^3)
h = max 42

data Shape = Circle Float Float Float | Rectangle Float Float Float Float
 deriving (Show)
area :: Shape -> Float
area (Circle _ _ r) = pi * r^2
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

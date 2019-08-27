module Demo where

import Data.Char

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


fibonacci6 n | n >=0 = helper6 0 1 n
fibonacci6 n | otherwise = error "abs shoud be > 0"

helper6 :: Integer -> Integer -> Integer -> Integer
helper6 acc1 acc2 0 = acc1
helper6 acc1 acc2 n = helper6 (acc2) (acc1 + acc2) (n-1)




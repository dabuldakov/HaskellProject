import Data.Char
import Data.Either
import Data.List.Split
-- :set +s


---------Kata ?

spisok :: [Char] -> [Char]
spisok x = replace (reverse x) "" (dlinaX x) ((reverse x) !! (dlinaX x)) 

dlinaX x = (length x) - 1

replace :: [Char] -> [Char] -> Int -> Char -> [Char]
replace list list2 (-1)  _ = list2
replace list list2 n 'A' = replace list (list2 ++ "T") (n-1) (list !! (n-1))
replace list list2 n 'T' = replace list (list2 ++ "A") (n-1) (list !! (n-1))
replace list list2 n 'G' = replace list (list2 ++ "C") (n-1) (list !! (n-1))
replace list list2 n 'C' = replace list (list2 ++ "G") (n-1) (list !! (n-1))


----------Kata: A Needle in the Haystack

findNeedle :: [[Char]] -> [Char]
findNeedle x = 
  let 
   helper :: [Char] -> Int -> [Char]
   helper xi (-1) = "none"
   helper "needle" n = "found the needle at position " ++ (show (n + 1))
   helper xi n = helper (x !! n) (n-1)
  in  helper "" (length x - 1)

----------

findNeedle' :: [[Char]] -> [Char]
findNeedle' x = helper' "" (length x - 1)
 where
   helper' :: [Char] -> Int -> [Char]
   helper' xi (-2) = "none"
   helper' "needle" n = "found the needle at position " ++ (show (n + 1))
   helper' xi n = helper' (x !! n) (n-1)

-------------Kata: Sum of positive

positiveSum :: [Int] -> Int
positiveSum st = sum [c | c <- st, c > 0]

-------------Kata: DNA to RNA Conversion

dnaToRna :: String -> String 
dnaToRna xs = replace' (reverse xs) "" (dlina xs) ((reverse xs) !! (dlina xs))

dlina x = (length x) - 1
replace' :: [Char] -> [Char] -> Int -> Char -> [Char]
replace' list list2 (-1)  _  = list2
replace' list list2 n    'T' = replace' list (list2 ++ "U") (n-1) (list !! (n-1))
replace' list list2 n    n2  = replace' list (list2 ++ [n2]) (n-1) (list !! (n-1))

dnaToRna' :: String -> String 
dnaToRna' x = map change x
 where 
  change 'T' = 'U'
  change c = c


dnaToRna'' :: String -> String 
dnaToRna'' = map (\c -> if c == 'T' then 'U' else c)

-----------Kata: Grasshopper - Summation

summation :: Integer -> Integer  
summation n = sum [1,2..n]

-----------Kata: Remove String Spaces

noSpace :: String -> String
noSpace = filter (/= ' ')

-----------Kata: Grasshopper - Grade book

getGrade :: Double -> Double -> Double -> Char
getGrade x y z = check ((x + y + z)/3)
 where 
   check m | m >= 0 && m < 60 = 'F'
   check m | m >= 60 && m < 70 = 'D'
   check m | m >= 70 && m < 80 = 'C'
   check m | m >= 80 && m < 90 = 'B'
   check m | m >= 90 && m <= 100 = 'A'

-----------Kata: Find numbers which are divisible by given number

divisibleBy :: [Int] -> Int -> [Int]
divisibleBy list c = [ x | x <- list, (mod x c) == 0]

divisibleBy' :: [Int] -> Int -> [Int]
divisibleBy' list x = filter (\q -> (mod q x) == 0) list

-----------Kata: Well of Ideas - Easy Version

well :: [String] -> String
well x | check > 2 = "I smell a series!"
       | check >= 1 = "Publish!"
       | otherwise = "Fail!"
 where 
  check = length (filter (`elem` ["good"]) x)
   
------------Kata: Fake Binary

fakeBin :: String -> String
fakeBin = map (\c -> if (read [c] :: Int) < 5 then '0' else '1')


fakeBin' :: String -> String
fakeBin' = map binarize
  where
   binarize :: Char -> Char
   binarize c | c < '5'   = '0'
              | otherwise = '1'
   
fakeBin'' :: String -> String
fakeBin'' xs = map (mutator '5') xs
  where
    mutator :: Char -> Char -> Char
    mutator threshold value
        | value < threshold = '0'
        | otherwise = '1'

------------Even or Odd

evenOrOdd :: Integral a => a -> [Char]
evenOrOdd n | (mod n 2) == 0 = "Even"
            | otherwise = "Odd"

evenOrOdd' :: Integral a => a -> [Char]
evenOrOdd' n
  | even n = "Even"
  | otherwise = "Odd"

--------------Grasshopper - Personalized Message

greet :: String -> String -> String
greet name owner | name == owner = "Hello boss"
                 | otherwise = "Hello guest"

--------------Get Nth Even Number

nthEven :: Int -> Int
nthEven x | x > 0 = last (take x [0,2..])
          | x < 0 = -(last (take (-x) [4,6..]))
          | x == 0 = -2

----------------Convert a Number to a String!

numberToString :: Int -> String
numberToString = show
          
-------------------Bin to Decimal

decToBin :: String -> Integer
decToBin s = read (reverse (binHelp (read s :: Integer) "")) :: Integer
 where 
   binHelp :: Integer -> [Char] -> [Char]
   binHelp x result | x == 1 = result ++ "1" 
                    | even x = binHelp (div x 2) (result ++ "0")
                    | odd x  = binHelp (div x 2) (result ++ "1")

------------------------------------

binToDec :: String -> Int
binToDec s = binHelp s ((length s) - 1) 0 (s !! ((length s) - 1))  0
  where
   binHelp :: String -> Int -> Int -> Char -> Int -> Int
   binHelp xs (-1) acc _ step = acc   
   binHelp xs   n  acc y step = binHelp xs (n-1) (acc + (*) (read [y] :: Int) (2^step)) (xs !! (n-1)) (step + 1)

----------------------------------

binToDec' :: String -> Int
binToDec' s =  sum (zipWith (\a b -> a * (2 ^ b))(reverse (map (\x -> read [x]::Int) s)) [0..])

---------------------------------------

binToDec'' :: String -> Int
binToDec'' s =  sum (zipWith (\a b -> read[a] * (2 ^ b))(reverse s) [0..])

---------------------Century From Year

century :: Int -> Int
century year 
  | snd x > 0 = fst x + 1
  | otherwise = fst x
    where
    x = divMod year 100

---------------------Expressions Matter 

expression x y z
   | x == 1 && z == 1 = x + y + z
   | x >= z = x * (max (y + z) (y * z))
   | z >= x = z * (max (x + y) (x * y))

------
expression' a b c = maximum [a+b+c, a*(b+c), a*b*c, a+(b*c), (a+b)*c]

----------------------String repeat

repeatStr :: Int -> String -> String
repeatStr n str = foldl (\acc x -> acc ++ x) "" str'
 where 
  str' = replicate n str

-----------
repeatStr' n s = concat $ replicate n s
repeatStr'' n = concat . replicate n

----------------------Count Odd Numbers below n
oddCount :: Int -> Int 
oddCount n = length [ c | c <- [1..n], odd c] -1
oddCount'' n = length [1,3..n] -1
oddCount' n = div n 2

---------------------Reversed Words

reverseWords :: String -> String 
reverseWords list = init (foldl (\acc x -> acc ++ x ++ " ") "" (reverse (check 1 list [] [])))
 where 
  check :: Int -> String -> [String] -> String -> [String]
  check n     []   list' word = list' ++ [word]
  check n (' ':xs) list' word = check   1   xs (list' ++ [word]) [] 
  check n   (x:xs) list' word = check (n+1) xs  list' (word ++ [x])

-----------------------

reverseWords' :: String -> String
reverseWords' = unwords . reverse . words
-------------------------
reverseWords'' :: String -> String
reverseWords'' x = unwords (reverse (words x))
----------------------------
--import Data.List
--reverseWords'' :: String -> String
--reverseWords'' s = intercalate " " (reverse $ words s)

--------------------Count by X

countBy :: Integer -> Int -> [Integer]
countBy x n = [x, x+x .. (x* toInteger(n))]
countBy' x n = take n [x, x + x..]
countBy'' x n = map (*x) [1..n]

--------------------Difference of Volumes of Cuboids

findDifference :: (Int, Int, Int) -> (Int, Int, Int) -> Int
findDifference (x, y, z) (a, b, c) = max (x*y*z) (a*b*c) - min (x*y*z) (a*b*c)

---------------------Simple Fun #1: Seats in Theater

seatsBlocked :: Int -> Int -> Int -> Int -> Int
seatsBlocked tot_cols tot_rows col row = x * y
 where
  x = (tot_cols - col +1)
  y = (tot_rows - row)

---------------------Is the string uppercase?

isUpperCase :: String -> Bool
isUpperCase []     = True
isUpperCase (x:xs) = if isLower x then False else isUpperCase xs

---isUpperCase' = not . any (`elem` ['a'..'z'])

---------------------Disemvowel Trolls

disemvowel :: String -> String
disemvowel = filter (\q -> not (q `elem` vowels'))
 where
  vowels = "aeiou" 
  vowels' = (map toUpper vowels) ++ vowels

disemvowel' = filter (`notElem` "AEIOUaeiou")

----------------------Sum Mixed Array

sumMix :: [Either String Int] -> Int
sumMix str = sum (map (\c -> (read c :: Int)) (lefts str)) + sum (rights str)
----------------------
sumMix' =sum . map f where
  f (Left x) = read x :: Int
  f (Right x) = x
  
------------------------Is there a vowel in there?

--a 97 e 101  i 105 o 111 u 117

isVow :: [Int] -> [Either Int String]
isVow ns = check ns ([] :: [Either Int String])
 where
  check [] acc = acc 
  check (97:xs)  acc = check xs (acc ++ [Right "a"])
  check (101:xs) acc = check xs (acc ++ [Right "e"])
  check (105:xs) acc = check xs (acc ++ [Right "i"])
  check (111:xs) acc = check xs (acc ++ [Right "o"])
  check (117:xs) acc = check xs (acc ++ [Right "u"])
  check (x:xs)   acc = check xs (acc ++ [Left x])
  
---------------------
----import Data.Char (chr)
isVow' = map (\i -> if i `elem` [97,101,105,111,117] then Right [chr i] else Left i)

---------------------------

isVow'' [] = []
isVow'' (x:xs)
    | elem toChar "aeiou" = Right [toChar] : isVow'' xs
    | otherwise = Left x : isVow'' xs
    where toChar = chr x

---------------------You only need one - Beginner

check :: Eq a => [a] -> a -> Bool
check [] y = False
check (x:xs) y = if (x == y) then True else check xs y

check' liste element = element `elem` liste

----------------------Is this my tail?

correctTail :: String -> Char -> Bool
correctTail bod tail = last bod == tail

----------------------Sum Arrays

sum' :: Num a => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: Num a => [a] -> a
sum'' q = helper q 0
 where 
  helper [] acc = acc
  helper (x:xs) acc = helper xs (acc + x)

----------------------Counting sheep...

countSheep :: [Bool] -> Int
countSheep xs = length (filter (== True) xs)  ---- быстрее в 3 раза чем вторйо метод

countSheep' :: [Bool] -> Int
countSheep' xs = length [ c | c <- xs, c == True]

countSheep'' = length . filter (==True)

----------------------Stringy Strings

stringy :: Int -> String
stringy n = take n (cycle "10")

----------------------If you can't sleep, just count sheep!!

countSheep2 :: Int -> String
countSheep2 n = cyclon n ""
  where 
   cyclon 0 acc = acc
   cyclon x acc = cyclon (x-1) ((show x) ++ " sheep..." ++ acc)

countSheep2' n = foldr (\x y -> show x ++ " sheep..." ++ y) "" [1..n]

---------------------Square(n) Sum

squareSum :: [Integer] -> Integer
squareSum xs = sum (map (^2) xs)

squareSum' :: [Integer] -> Integer
squareSum' xs = sum [c^2 | c <- xs]

squareSum'' :: [Integer] -> Integer       ------ самый быстрый
squareSum'' = foldr (\x s -> x*x + s) 0  

squareSum''' :: [Integer] -> Integer
squareSum''' [] = 0
squareSum''' (x:xs) = x ^ 2 + squareSum xs

---------------------How many lightsabers do you own?

howManyLightsabersDoYouOwn :: Num a => [Char] -> a
howManyLightsabersDoYouOwn "Zach" = 18
howManyLightsabersDoYouOwn _ = 0

howManyLightsabersDoYouOwn' :: Num a => [Char] -> a
howManyLightsabersDoYouOwn' x = case x of 
  "Zach" -> 18
  _ -> 0

-------------------Function 1 - hello world

greet2 :: String
greet2 = map (chr) [104,101,108,108,111,32,119,111,114,108,100,33]

greet2' :: String
greet2' = foldl (\s x -> s ++ [(chr x)]) "" [104,101,108,108,111,32,119,111,114,108,100,33] 

---------------------Float Precision

solution ::Float -> Float
solution x = fromIntegral (round (x * 100)) / 100.0

{-import Text.Printf

solution ::Float -> Float
solution = read . printf "%.2f"-}

---------------------Largest 5 digit number in a series

digit5 :: String -> Int
digit5 a = check a 0
 where
  check (x:y:z:k:[]) acc = acc
  check (x:y:z:k:l:xs) acc = check (y:z:k:l:xs) (max (read (x:y:z:k:l:"") :: Int) acc)
-------------------------
digit55 :: String -> Int
digit55 a = check a 0
 where
  check (x:y:z:k:[]) acc = acc
  check ys acc = check (tail ys) (max (read (take 5 ys) :: Int) acc)
---------------------------  
digit555 :: String -> Int
digit555 = maximum . map read . string5
  where string5 [] = []
        string5 ys = (take 5 ys):(string5 (tail ys))
--------------------------------
digit5' :: String -> Int
digit5' = maximum . map read . divvy 5 1
-----------------------------------
digit5'' :: String -> Int
digit5'' xs = maximum (map read (divvy 5 1 xs))

------------------------Maximum Multiple

maxMultiple :: Int -> Int -> Int
maxMultiple x y = (div y x) * x

-------------------------Alphabetical Addition

addLetters :: [Char] -> Char
addLetters xs = chr (check (sum (map (\a -> ((ord a) - 96)) xs))+ 96)
 where
  check a 
          | a == 0 || (mod a 26) == 0 = 26
          | otherwise = a - ((div a 26) * 26)
-----------------------

addLetters' :: [Char] -> Char
addLetters' xs = snd $ head (getChar getSum)
 where
  getChar z | z == 0 || (mod z 26) == 0 = [(0, 'z')]
            | otherwise = filter (\(a,b) -> (z - ((div z 26) * 26)) == a) list 
  getSum = sum (map (\q -> if (snd q) `elem` xs then (fst q) else 0) list)
  list = zip [1..] ['a'..'z']

------------------------Shortest Word

find_shortest :: String -> Integer
find_shortest = toInteger . minimum . map length . words

------------------------Simple Fun #2: Circle of Numbers

circleOfNumbers :: Int -> Int -> Int
circleOfNumbers n fn | check - fn == 0 = 0
                     | check + fn < n = check + fn
                     | check + fn > n = fn - check
 where check = div n 2

circleOfNumbers' m i 
  | i < n = i + n
  | otherwise = i - n
  where n = m `div` 2

---------------------------------Remove duplicate words

removeDuplicateWords :: String -> String
removeDuplicateWords = unwords (check words [])
 where check   []   y = y 
       check (x:xs) y = if (filter (== x) y) == [] then check xs (y ++ x) else check xs y  
  

































 

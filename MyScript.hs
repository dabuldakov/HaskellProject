import Data.Char
import Data.Either
import Data.List.Split
import Data.List
import Data.Bool
import Text.Read (readMaybe)
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

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort a ++ [x] ++ quickSort b 
 where
  a = filter (\q -> q <= x) xs 
  b = filter (\q -> q > x) xs

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

----------------------Row Weights

rowWeights :: [Int] -> [Int]
rowWeights x = 
 let
  check _ [] = []
  check n (x:xs) = if odd n then x : check (n+1) xs else check (n+1) xs
  acc = sum (check 1 x)
 in 
  [acc] ++ [sum x - acc]

rowWeights' :: [Int] -> [Int]
rowWeights' x = let list = sum $ zipWith (*) x (cycle [1,0])
                in  [list] ++ [sum x - list]

rowWeights'' :: [Int] -> [Int]
rowWeights'' [] = [0,0]
rowWeights'' [x] = [x,0]
rowWeights'' (x:y:xs) = zipWith (+) [x,y] (rowWeights xs)
 
rowWeights''' :: [Int] -> [Int]
rowWeights''' list = [sum $ team odd, sum $ team even]
  where
    team f = [list !! (x - 1) | x <- [1 .. length list], f x]

---------------Form The Minimum

minValue :: [Int] -> Int
minValue = read . concat . map show . nub . sort

--minValue' =  read . concatMap show. sort. nub

---------------Minimum Steps (Array Series #6)

minimumSteps :: [Int] -> Int -> Int
minimumSteps list k = check (sort list) 0
 where
  check     [] acc = -1
  check (x:xs) acc | acc >= k = -1
                   | otherwise = check xs (acc + x) + 1

minimumSteps' :: [Int] -> Int -> Int
minimumSteps' xs n = length $ takeWhile (< n) $ scanl1 (+) $ sort xs

---------------Maximum Product 

adjacentElementProduct :: [Integer] -> Integer
adjacentElementProduct = check
 where 
   check   (y:[]) = 0
   check (x:y:xs) = max (x * y) (check $ y:xs)

adjacentElementProduct' :: [Integer] -> Integer
adjacentElementProduct' x = maximum $ zipWith (*) x $ tail x 

--------------Nth Smallest Element (Array Series #4) 

nthSmallest :: [Int] -> Int -> Int
nthSmallest list x = sort list !! (x-1)

---------------Transform To Prime

minimumNumber :: [Integer] -> Integer
minimumNumber n = check 0 $ sum n
 where check i k | simpleNumber k = i
                 | otherwise = check (i+1) (k+1)  

simpleNumber :: Integer -> Bool
simpleNumber x = not . elem 0 $ map (mod x) [2..k]
 where 
  k = truncate . sqrt $ fromIntegral x

-----------------Balanced Number (Special Numbers Series #1 ) 

balancedNum' :: Int -> String
balancedNum' x = if checkOdd then "Balanced" else "Not Balanced"
 where
 checkOdd | mod size 2 == 0 = showBalance
          | otherwise = showBalance1
 showBalance = (sum $ init $ fst check) == tailList
 showBalance1 = (sum $ fst check) == tailList
 tailList = (sum $ tail $ snd check)
 check = splitAt (div size 2) (map digitToInt $ show x)
 size = length $ show x
 
balancedNum'' :: Int -> String
balancedNum'' = bool "Not Balanced" "Balanced" . isBalanced
  where
    isBalanced n = 
        let digits = map digitToInt $ show n
            m = (length digits - 1) `div` 2
            s = sum . take m
        in s digits == s (reverse digits)

-----------------STRONGN Strong Number (Special Numbers Series #2) 

strong :: Integer -> String
strong x = if fromIntegral (sum . map (factorial . digitToInt) $ show x) == x then "STRONG!!!!" else "Not Strong !!"
 where
  factorial 0 = 1
  factorial n = n * factorial (n-1)

------------------Disarium Number (Special Numbers Series #3)

disariumNumber :: Int -> String
disariumNumber n = if sum (zipWith (\a b -> (digitToInt a) ^ b) (show n) [1..]) == n then "Disarium !!" else "Not !!"

disariumNumber' n =  let s = sum (map (\(x,i) -> (digitToInt x) ^ i) (zip (show n) [1..]))
                    in if s == n then "Disarium !!" else "Not !!"

------------------Jumping Number (Special Numbers Series #4)

jumpingNumber :: Int -> String
jumpingNumber x = if abs (product . zipWith (-) s $ tail s) > 1 then "Not!!" else "Jumping!!"
 where 
  s = map digitToInt $ show x

------------------Special Number (Special Numbers Series #5)

specialNumber :: Int -> String
specialNumber x =  if False `elem` (map (`elem` [0..5]) s) then "NOT!!" else "Special!!"
  where 
  s = map digitToInt $ show x

specialNumber' n =
  if all (<= '5') . show $ n
  then "Special!!" else "NOT!!"

------------------Automorphic Number (Special Numbers Series #6)

automorphic :: Integer -> String
automorphic x = if any (==False) $ zipWith (==) (reverse $ s x) (reverse $ s $ x^2) then "Not!!" else "Automorphic"
 where 
  s y = map digitToInt $ show y

automorphic' n = bool "Not!!" "Automorphic" $ show n `isSuffixOf` show (n^2)

------------------Extra Perfect Numbers (Special Numbers Series #7)

extraPerfect :: Int -> [Int]
extraPerfect x = filter (/=0) (map (\a -> if (head (decToBin' a)) == (last (decToBin' a)) then a else 0) [1..x])

decToBin' :: Int -> [Char]
decToBin' s = reverse (binHelp s "")
 where 
   binHelp :: Int -> [Char] -> [Char]
   binHelp x result | x == 1 = result ++ "1" 
                    | even x = binHelp (div x 2) (result ++ "0")
                    | odd x  = binHelp (div x 2) (result ++ "1")

-------------------Tidy Number (Special Numbers Series #9)

tidyNumber :: Int -> Bool
tidyNumber = check . show
  where  
   check :: [Char] -> Bool
   check   (y:[]) = True
   check (x:y:xs) | x <= y = check $ y:xs
   check _        | otherwise = False

tidyNumber' :: Int -> Bool
tidyNumber' x =  and $ zipWith (<=) (show x) (tail $ show x)

------------------Primorial Of a Number 

numPrimorial :: Int -> Integer
numPrimorial n = product $ take n $ filter (>0) $ map (\a -> if simpleNumber a then a else 0) [2..]

simpleNumber1 :: Integer -> Bool
simpleNumber1 x = not . elem 0 $ map (mod x) [2..k]
 where 
  k = truncate . sqrt $ fromIntegral x

numPrimorial' :: Int -> Integer
numPrimorial' n = product $ take n $ prime [2..]
  where prime (p:ps) = p : prime [x | x <- ps, mod x p /= 0]

------------------String ends with?

solution1 :: String -> String -> Bool
solution1 x y = isSuffixOf y x 

solution' :: String -> String -> Bool
solution' = flip isSuffixOf

------------------Square Every Digit

squareDigit :: Int -> Int
squareDigit x | x < 0 =  - f
              | otherwise = f
 where
  f = read $ concat $ map (\a -> show $ a^2) s
  s = map digitToInt $ show $ abs x

--------------------Functions of Integers on Cartesian Plane

sumin :: Integer -> Integer
sumin n = sum $ scanl (-) (sum [1..n]) [1..(n-1)]

sumax :: Integer -> Integer
sumax n = sum $ scanl (-) (n^2) [(n-1),(n-2)..1]

sumsum :: Integer -> Integer
sumsum n = sumin n + sumax n

--------------------The Poet And The Pendulum

pendulum :: [Int] -> [Int]
pendulum n = (reverse $ generate [0,1]) ++ [head $ list] ++ generate [1,0]
 where 
  list = sort n
  generate m = filter (/=0) $ zipWith (*) (tail $ list) $ cycle m


pendulum' :: [Integer] -> [Integer] 
pendulum' = check 0 [] . sort
  where 
   check k acc [] = acc
   check k acc (x:xs) | k == 0    = check (k+1) (x:acc) xs
                      | odd k     = check (k+1) (acc ++ [x]) xs
                      | otherwise = check (k+1) (x:acc) xs
  
pendulum'' :: [Integer] -> [Integer]
pendulum'' = check 0 [] [] . sort
  where 
   check k accL accR [] = accL ++ (reverse accR)
   check k accL accR (x:xs) | k == 0    = check (k+1) (x:accL)    accR  xs
                            | odd k     = check (k+1)    accL  (x:accR) xs
                            | otherwise = check (k+1) (x:accL)    accR  xs

pendulum''' :: [Int] -> [Int]
pendulum''' xs = map (sorted !!) indices where
  l = length xs - 1
  indices = reverse [0,2..l] ++ [1,3..l]
  sorted = sort xs


-------------------------Is this a triangle?

isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c | a < (b + c) && a > (b - c) &&
                   b < (a + c) && b > (a - c) && 
                   c < (a + b) && c > (a - b) = True
                 | otherwise = False

-------------------------Highest and Lowest

highAndLow :: String -> String
highAndLow input = show (maximum check) ++ " " ++ show (minimum check) 
 where 
  check =  map (read) (words input) :: [Int]

--------------------Isograms

isIsogram :: String -> Bool
isIsogram = check . map toLower
 where 
  check [] = True
  check (x:xs) | x `elem` xs = False
  check (x:xs) | otherwise = check xs

isIsogram' :: String -> Bool
isIsogram' n = n == (nub $ map toLower n)

-------------------Split Strings

solution2 :: String -> [String]
solution2 xs = check $ xs ++ "_" 
 where
  check xss = let l = length xss - 1
              in zipWith (\a b -> xss !! a : xss !! b : []) [0,2..l] [1,3..l]

solution2' [] = []
solution2' (x:[]) = [[x,'_']]
solution2' (x:y:xs) = [x,y]:(solution2' xs)

-------------------Tribonacci Sequence

tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci (a,b,c) 0 = []
tribonacci (a,b,c) 1 = [a]
tribonacci (a,b,c) 2 = [a,b]
tribonacci (a,b,c) n = check [c,b,a] 4
 where 
  check            line i | i > n = reverse line
  check line@(x:y:z:xs) i | otherwise = check ((x+y+z):line) (i+1)

tribonacci' :: Num a => (a, a, a) -> Int -> [a]
tribonacci' _ n | n < 1 = []
tribonacci' (a, b, c) n = a : tribonacci (b, c, a+b+c) (n-1)

------------------Title Case

titleCase :: String -> String -> String
titleCase _ [] = []
titleCase minor title = (toUpper $ head check) : tail check
 where 
  check = unwords (map (\a -> if elem a lm then (toLower $ head a) : tail a else a) lt)
  lm = lower minor
  lt = lower title
  lower x =  map (\a -> (toUpper (head a) : (map toLower $ tail a))) (words x)

--------------------Dubstep

songDecoder :: String -> String
songDecoder = unwords . words . check
 where
  check [] = []
  check (x:[]) = x:[]
  check (x:y:[]) = x:y:[]
  check (x:y:z:xs) = if x == 'W' && y == 'U' && z == 'B' then (' ' : (check xs)) else (x : check (y:z:xs))

songDecoder' :: String -> String
songDecoder' = unwords . words . go
  where go []               = []
        go ('W':'U':'B':xs) = ' ' : go xs
        go (x:xs)           =   x : go xs

songDecoder'' :: String -> String
songDecoder'' = unwords . filter (not . null) . splitOn "WUB"

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

reverseWords2 :: String -> String
reverseWords2 n = check n [] []
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

----------------Parse float

parseFloat :: String -> Maybe Float
parseFloat [] = Nothing
parseFloat s | check = Nothing
             | otherwise = Just (read s :: Float)
 where 
  check = any (==False) (map (\a -> (isDigit a) || (a == '.')) s)

parseFloat' :: String -> Maybe Float
parseFloat' = readMaybe

---------------Odder Than the Rest

oddOne :: [Int] -> Int
oddOne = check 0 
 where 
  check n []  = -1
  check n (x:xs) | odd x = n
                 | otherwise = check (n+1) xs  

--oddOne :: [Int] -> Int
--oddOne = fromMaybe (-1) . findIndex odd

-------------------Sort Out The Men From Boys

menFromBoys :: [Int] -> [Int]
menFromBoys xs = check even ++ (reverse $ check odd)
 where 
  check f = sort $ filter f $ nub xs


-------------------Separate The Wheat From The Chaff

wheatFromChaff :: [Int] -> [Int]
wheatFromChaff n = snd $ unzip $ sort (check ++ myZip x)
 where 
  x = zip [0..] n
  y = filter (\a -> snd a > 0) x
  z = reverse $ filter (\a -> snd a < 0) x
  check = (zipWith (\a b -> if (fst a) < (fst b) then (fst a, snd b) else a) y z) ++ (zipWith (\a b -> if (fst a) < (fst b) then (fst b, snd a) else b) y z)
  check' = fst $ unzip check
  myZip [] = []
  myZip (x:xs) | (fst x) `elem` check' = myZip xs
               | otherwise = x : myZip xs   
-----------------------------------------------

wheatFromChaff' :: [Int] -> [Int]
wheatFromChaff' n = check n list []
 where
  list = reverse $ filter (<0) n
  check []     [] _ = []
  check (x:xs) [] z     |     x > 0 =       x  : (check xs []       z )
                        | otherwise = (head z) : (check xs [] (tail z)) 

  check (x:xs) (y:ys) z |     x > 0 = y : (check xs          ys  (x:z))
                        | otherwise = x : (check xs (init (y:ys))   z )                                                                       
-----------------------------------------------

wheatFromChaff''' :: [Int] -> [Int]   
wheatFromChaff''' n = check n list []
 where
  list = reverse $ filter (<0) n

  check2    []    _    = []
  check2    xs     []  = xs
  check2 (x:xs) (z:zs) |     x > 0 = x : (check2 xs (z:zs))
                       | otherwise = z : (check2 xs    zs ) 

  check x      []     z     = check2 x z
  check (x:xs) (y:ys) z     |     x > 0 = y : (check xs          ys (x:z))
                            | otherwise = x : (check xs (init (y:ys)) z )  
------------------------------------------

wheatFromChaff'' :: [Int] -> [Int]              -----прошел тест таймаута
wheatFromChaff'' n = check n list [] ll
 where
  ll = length list
  list = reverse $ filter (<0) n
  
  check2 []     _          = []
  check2 xs     []         = xs
  check2 (x:xs) (z:zs)     | x > 0     = x : (check2 xs (z:zs))
                           | otherwise = z : (check2 xs    zs ) 

  check x         y    z 0 = check2 x z
  check (x:xs) (y:ys)  z k | x > 0     = y : (check xs    ys (x:z) (k-1))
                           | otherwise = x : (check xs (y:ys)   z  (k-1)) 

--------------------Merged String Checker
isMerge :: String -> String -> String -> String
isMerge s part1 part2 = check part1 part2 
 where
  check :: String -> String -> String
  check [] []         = []
  check x  []         = [] ++ x
  check []  y         = [] ++ y
  check (x:xs) (y:ys) = x: y : (check xs ys) 

---------------------Equal Sides Of An Array

findEvenIndex :: [Int] -> Int
findEvenIndex arr =  check arr 0 0
 where
  check []     left i | left == 0 = i 
                      | otherwise = (-1)
  check (x:xs) left i | left == sum xs = i
                      | otherwise = check xs (left + x) (i+1) 

----------------------Simple prime streaming

solve :: Int -> Int -> String 
solve a b = drop a $ take (a+b) $ concatMap show $ filter simpleNumber2 [2..]

simpleNumber2 :: Integer -> Bool
simpleNumber2 x = not . elem 0 $ map (mod x) [2..k]
 where 
  k = truncate . sqrt $ fromIntegral x




















 

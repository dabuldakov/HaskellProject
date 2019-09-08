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

------------Is there a vowel in there?
--a 97 e 101  i 105 o 111 u 117

isVow :: [Int] -> [Either Int String]
isVow ns = error "y n ocodez?"


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

reverseWords :: String -> [String] 
reverseWords list = foldl (\acc x -> acc ++ x ++ " ") "" (reverse (check 1 list [] []))
 where 
  check :: Int -> String -> [String] -> String -> [String]
  check n     []   list' word = list' ++ [word]
  check n (' ':xs) list' word = check 1 xs (list' ++ [word]) [] 
  check n   (x:xs) list' word = check (n+1) xs list' (word ++ [x])
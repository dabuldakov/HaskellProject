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


 

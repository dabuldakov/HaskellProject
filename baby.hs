doubleMe x = x + x 
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

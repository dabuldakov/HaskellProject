import Data.Char
import Data.List
import qualified Data.Map as Map
import Text.Read (readMaybe)

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

data Person = Person { firstName :: String 
                     , lastName :: String 
                     , age :: Int 
                     , heigt :: Float 
                     , phoneNumber :: String
                     , flavor :: String } deriving (Show)

guy = Person "Fred" "Kruger" 44 190.5 "22-45-45" "Escimo"

fl :: Person -> String
fl (Person f l _ _ _ _) = f ++ l

-------------------------

data Car = Car {company :: String
              , model :: String
              , year :: Int } deriving (Show)

tellCar :: Car -> String
tellCar (Car c n y) = c ++ " " ++ n ++ " " ++ show y

mustang = Car "Ford" "Mustang" 1978

-----------------------------

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
vplus (Vector i j k) (Vector l m n) = Vector (i+l) (j+m) (k+n)

scalarProd :: (Num a) => Vector a -> Vector a -> a
scalarProd (Vector i j k) (Vector l m n) = i*l + j*m + k*n

vmult :: (Num a) => Vector a -> a -> Vector a
vmult (Vector i j k) m = Vector (i*m) (j*m) (k*m)

-----------------------------

data Person2 = Person2 {firstName2 :: String
                      , lastName2 :: String
                      , age2 :: Int } deriving (Eq, Show, Read)

bob = Person2 "Bob" "Dilan" 55
john = Person2 "John" "Brein" 25
mark = Person2 "Mark" "Tven" 70

mysteryDude = "Person2 { firstName2 =\"Mikle\"" ++
                     ", lastName2 =\"Dimond\"" ++
                     ", age2 = 45}"

------------------------------------

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
 deriving (Eq, Ord, Show, Read, Bounded, Enum)

-------------------------------------

phoneBook'' :: PhoneBook
phoneBook'' = 
 [("a","555-29-38")
 ,("s","452-29-28")
 ,("d","493-29-28")
 ,("f","205-29-28")
 ,("f","222-29-28")
 ,("gg","939-82-82")
 ,("gg","853-24-92")]


type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook namep numberp bookp = (namep, numberp) `elem` bookp

----------------------------------------------

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = 
  case Map.lookup lockerNumber map of
   Nothing            -> Left $ "Bar: " ++ show lockerNumber ++ " no exist!" 
   Just (state, code) -> if state /= Taken then Right code else Left $ "Bar: " ++ show lockerNumber ++ " not free!"
  
lockers :: LockerMap
lockers = Map.fromList
 [(100,(Taken,"ZD39I"))
 ,(101,(Free,"JAH3I"))
 ,(103,(Free,"IQSA9"))
 ,(105,(Free,"QOTSA"))
 ,(109,(Taken,"893JJ"))
 ,(110,(Taken,"99292"))
 ]

----------------------------------------------

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 ^++
(^++) :: List a -> List a -> List a
Empty ^++ ys = ys 
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)

-------------------------------------------------

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
 | x == a = Node x left right
 | x < a = Node a (treeInsert x left) right
 | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
 | x == a = True
 | x < a = treeElem x left
 | x > a = treeElem x right

----------------------------

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
 Red == Red = True
 Green == Green = True
 Yellow == Yellow = True
 _ == _ = False

instance Show TrafficLight where
 show Red = "Red color"
 show Yellow = "Yellow color"
 show Green = " Green color"

data TrafficAuto = RedAuto | YellowAuto | GreenAuto deriving (Eq, Show)

-------------------------------

class YesNo a where
 yesno :: a -> Bool

instance YesNo Int where
 yesno 0 = False
 yesno _ = True

instance YesNo [a] where
 yesno [] = False
 yesno _ = True

instance YesNo Bool where
 yesno = id

instance YesNo (Maybe a) where
 yesno (Just _) = True
 yesno Nothing = False

instance YesNo (Tree a) where
 yesno EmptyTree = False
 yesno _ = True

instance YesNo TrafficLight where
 yesno Red = False
 yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

---------------------------------------

instance Functor Tree where
 fmap f EmptyTree = EmptyTree
 fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

------------------------------------------

data Frank a b = Frank {frankField :: b a} deriving (Show)

class Tofu t where
 tofu :: j a -> t a j

instance Tofu Frank where
 tofu x = Frank x


---------------------- 8. ВВОД-ВЫВОД

-------------------Separate The Wheat From The Chaff

wheatFromChaff :: [Int] -> [(Int, Int)]
wheatFromChaff n = check
 where 
  x = zip [0..] n
  y = filter (\a -> snd a > 0) x
  z = reverse $ filter (\a -> snd a < 0) x
  check = (zipWith (\a b -> if (fst a) < (fst b) then (fst a, snd b) else a) y z) ++ (zipWith (\a b -> if (fst a) < (fst b) then (fst b, snd a) else b) y z)































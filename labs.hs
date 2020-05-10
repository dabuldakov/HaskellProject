import Data.Time

--------------------------------ЛАБОРАТОРНАЯ РАБОТА 1. ОСНОВЫ ЯЗЫКА ПРОЛОГ. СОЗДАНИЕ ПРОСТЕЙШИХ ФУНКЦИЙ

odd' []       = []
odd' (x:[])   = []
odd' (x:y:xs) = y : odd' xs

---------------------------------ЛАБОРАТОРНАЯ РАБОТА 2. РАЗРАБОТКА ПРОГРАММ.

replace' []     []     list = list
replace' (x:xs) (y:ys) list = replace' xs ys (map (\a -> if a == x then y else a) list)

---------------------------------ЛАБОРАТОРНАЯ РАБОТА 4. РАБОТА С БД.
x = [words ("7171 Krasnoyarsk Tomsk 06/04/2020/15/00 06/04/2020/19/00"), words ("1177 Tomsk Kemerovo 06/04/2020/12/50 06/04/2020/13/15"), words ("1113 Tomsk Moscow 06/04/2020/11/40 06/04/2020/15/30")]
 
flyes:: String -> String -> [[String]] -> [[String]]
flyes []   [] data_list = data_list
flyes from [] data_list = filter (\a -> (a !! 1) == from)                   data_list
flyes []   to data_list = filter (\a -> (a !! 2) == to)                     data_list
flyes from to data_list = filter (\a -> (a !! 1) == from && (a !! 2) == to) data_list


---------------------

mainList = diff_data_list (flyes "Tomsk" [] x)

diff_data_list []   = []
diff_data_list (x:xs) = [(x !! 0), (show (diffUTCTime (check (x !! 3)) (check (x !! 4))))] : diff_data_list xs--
	where
		check  r = timeS (readS r 2) (readS r 1) (readS r 0) (readS r 3) (readS r 4) 0
		helper s = split '/' s
		readS  p n  = read ((helper p) !! n) :: Int

timeS y m d h min s = UTCTime (fromGregorian y m d) (sec h min s)

sec h m s = h*60*60 + m*60 + s

--diffTime y2 m2 d2 h2 m2 s2 y1 m1 d1 h1 m1 s1 = diffUTCTime (UTCTime (fromGregorian y2 m2 d2) (sec h2 m2 s2) (UTCTime (fromGregorian y1 m1 d1) (sec h1 m1 s1))



-------------------------
split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s
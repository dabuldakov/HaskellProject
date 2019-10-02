
data Section = Section { getA :: Int,
                         getB :: Int,
                         getC :: Int } deriving (Show)

type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [ Section 50 10 30
                   , Section  5 90 20
                   , Section 40  2 25
                   , Section 10  8  0 ]

data Label = A | B | C deriving (Show)
type Path  = [(Label, Int)]

optimalPath :: RoadSystem -> Path
optimalPath roadSystem = 
  let (bestApath, bestBpath) = foldl roadStep ([],[]) roadSystem
  in if sum (map snd bestApath) <= sum (map snd bestBpath)
        then reverse bestApath
        else reverse bestBpath


groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)


roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) = 
  let timeA = sum $ map snd pathA
      timeB = sum $ map snd pathB
      forwardTimeToA = timeA + a
      crossssTimeToA = timeB + b + c
      forwardTimeToB = timeB + b
      crossssTimeToB = timeA + a + c
      newPathToA = if forwardTimeToA <= crossssTimeToA
                      then       (A,a):pathA
                      else (C,c):(B,b):pathB
      newPathToB = if forwardTimeToB <= crossssTimeToB
                      then       (B,b):pathB
                      else (C,c):(A,a):pathA
  in (newPathToA, newPathToB)

main = do
  contents <- getContents
  let threes = groupsOf 3 (map read $ lines contents)
      roadSystem = map (\[a,b,c] -> Section a b c) threes
      path = optimalPath roadSystem
      pathString = concat $ map (show. fst) path
      pathTime = sum $ map snd path
  putStrLn $ "Best path: " ++ pathString
  putStrLn $ "Time: " ++ show pathTime










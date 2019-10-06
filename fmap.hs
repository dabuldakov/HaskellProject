import qualified Data.List as L
import Data.Char
import Control.Monad



{-main = do
 a <- (++) <$> getLine <*> getLine
 putStrLn $ "Two lines: " ++ a-}
 
--------------------Cycle a list of values

data Direction = L | R deriving (Show, Eq)

cycleList :: (Eq a) => Direction -> [a] -> a -> Maybe a
cycleList d list p | d == R = check list'
                   | d == L = check $ reverse list'
 where
  check l = (!!) <$> (Just l) <*> p' l
  p'    l = pure (succ) <*> (L.findIndex (== p) l)
  list'   = list ++ list

-------------------

cycleList' :: (Eq a) => Direction -> [a] -> a -> Maybe a
cycleList' d l v = lookup v $ case d of
                               L -> zip l' l
                               R -> zip l l'
  where l' = tail $ cycle l

-------------------

cycleList'' :: (Eq a) => Direction -> [a] -> a -> Maybe a
cycleList'' d l v = liftM (\i -> (l!!) . (`mod` length l) . (length l +) . (if d == R then succ else pred) $ i) . L.elemIndex v $ l


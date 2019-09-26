module SeqMy where

import Data.Sequence
import qualified Data.Sequence as Seq

{-wheatFromChaff :: [Int] -> [Int]
wheatFromChaff n = check $ Seq.fromList n 2
 where 
  list = Seq.reverse $ Seq.filter (<0) n
  ll = Seq.length list
  ln = Seq.length n
  leftI x = Seq.length $ Seq.takeWhileL (<0) x
  rightI x = ln - (Seq.length $ Seq.takeWhileR (>0) nums) - 1
  leftX = Seq.index n leftI
  rightX = Seq.index n rightI
  check x 0 = x
  check x y = check (update rightI leftX (update leftI rightX x)) (y-1)-}
  

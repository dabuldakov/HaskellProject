import Data.Char
import Control.Monad
import System.IO

{-main = do
  putStrLn "Enter words: "
  enterWords <- getLine
  if null enterWords 
    then do
    putStrLn "none" 
    putChar 'q'
    return ()
    else do
      putStrLn $ reverseWords enterWords
      main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words -}

{-main2 = do
input <- getLine
when (input == "РЫБА-МЕЧ") $ do
putStrLn input-}

{-main = do
 rs <- sequence [getLine, getLine, getLine]
 print rs-}

{-main = do
 colors <- forM [1,2,3,4] (\a -> do
  putStrLn $ "WIth what color: " ++ show a ++ "?"
  color <- getLine
  return color)
 putStrLn "Colors association 1, 2, 3, 4: "
 mapM_ putStrLn colors-}

{-main = interact isPalindrom

shortLines :: String -> String
shortLines = unlines . filter (\a -> length a < 15) . lines

isPalindrom :: String -> String
isPalindrom =
 unlines .
 map (\a -> if isPal a then "palindrom" else "not palindrom" ) .
 lines

isPal x = x == reverse x-}

{-main = do
 handle <- openFile "file.txt" ReadMode
 contents <- hGetContents handle
 putStr contents
 hClose handle-}

main = do
 withFile "file.txt" ReadMode (\h -> do 
  contents <- hGetContents h
  putStr contents)











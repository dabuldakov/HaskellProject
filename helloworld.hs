import Data.Char
import Control.Monad
import Control.Exception
import System.IO
import System.Directory
import System.Environment 
import Data.List


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

{-main = do
 withFile "file.txt" ReadMode (\h -> do 
  contents <- hGetContents h
  putStr contents)-}
  
{-main = do 
 contents <- readFile "file.txt" 
 putStr contents-}
 
{-main = do
 contents <- readFile "file.txt"
 appendFile "file1.txt" (map toUpper contents)-}
 
{-main = do
 todoItem <- getLine 
 appendFile "todo.txt" (todoItem ++ "\n")-}
 
{-main = do
 contents <- readFile "todo.txt"
 let todoTasks = lines contents
     numberedTasks = zipWith (\a b -> show a ++ ") " ++ b) [0..] todoTasks
 putStrLn "Your tasks:"
 mapM_ putStrLn numberedTasks
 putStrLn "Choose number for delete!"
 numberString <- getLine
 let number = read numberString
     newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
 bracketOnError (openTempFile "." "temp")
   (\(tempName, tempHandle) -> do
        hClose tempHandle
        removeFile tempName)
   (\(tempName, tempHandle) -> do
        hPutStr tempHandle newTodoItems
        hClose tempHandle
        removeFile "todo.txt"
        renameFile tempName "todo.txt"
        putStrLn ("Deleted: " ++ (todoTasks !! number))) -}

dispatch :: String -> [String] -> IO () 
dispatch "add"    = add 
dispatch "view"   = view
dispatch "remove" = remove
dispatch "bump"   = bump
dispatch command  = doesntExist command

doesntExist :: String -> [String] -> IO ()
doesntExist command _ = putStrLn $ "Command " ++ command ++ " not found"


main = do  
  (command:argList) <- getArgs   
  dispatch command argList


add :: [String] -> IO () 
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _ = putStrLn "Command ADD has only two argements"

view :: [String] -> IO ()
view [fileName] = do
 contents <- readFile fileName
 let todoTasks = lines contents
     numberedTasks = zipWith (\n line -> show n ++ ") " ++ line) 
                             [0..] 
                             todoTasks 
 putStr $ unlines numberedTasks
view _ = putStrLn "Command ADD has one argements"
 
remove :: [String] -> IO ()
remove [fileName, numberString] = do
 contents <- readFile fileName
 let todoTasks = lines contents
     number = read numberString
     newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
 bracketOnError (openTempFile "." "temp")
  (\(tempName, tempHandle) -> do
        hClose tempHandle
        removeFile tempName) 
  (\(tempName, tempHandle) -> do
        hPutStr tempHandle newTodoItems
        hClose tempHandle
        removeFile fileName
        renameFile tempName fileName)
remove _ = putStrLn "Command ADD has only two argements"

bump :: [String] -> IO ()
bump [fileName, numberString] = do
 contents <- readFile fileName
 let todoTasks = lines contents
     number = read numberString
     upItem = todoTasks !! number
     newTodoItems = unlines (upItem : (delete upItem todoTasks))
 bracketOnError (openTempFile "." "temp")
  (\(tempName, tempHandle) -> do
        hClose tempHandle
        removeFile tempName) 
  (\(tempName, tempHandle) -> do
        hPutStr tempHandle newTodoItems
        hClose tempHandle
        removeFile fileName
        renameFile tempName fileName)
bump _ = putStrLn "Command ADD has only two argements"











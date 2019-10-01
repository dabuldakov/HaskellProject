import Data.Char
import Control.Monad
import Control.Exception
import System.IO
import System.IO.Error
import System.Directory
import System.Environment 
import Data.List
import qualified Data.ByteString.Lazy as B
import Prelude hiding (catch)


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

{-dispatch :: String -> [String] -> IO () 
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
bump _ = putStrLn "Command ADD has only two argements" -}

{-main = do 
 (fileName1:fileName2:_) <- getArgs 
 copy fileName1 fileName2

copy :: FilePath -> FilePath -> IO ()
copy source dest = do
 contents <- B.readFile source
 bracketOnError
   (openTempFile "." "temp")
   (\(tn, th) -> do
     hClose th
     removeFile tn)
   (\(tn, th) -> do
     B.hPutStr th contents
     hClose th
     renameFile tn dest)-}
 
--------------------Exception

{-params :: [String] -> (Integer, Integer)
params [a,b] = (read a, read b)

printQuotients :: Integer -> Integer -> IO ()
printQuotients a b = do
 print $ a `divMod` b
 print $ b `divMod` a

main = do 
  args <- getArgs  
  let (a,b) = params args
  res <- try (printQuotients a b) :: IO (Either ArithException ())
  case res of
    Left e -> putStrLn "Div zerro!"
    Right () -> putStrLn "OK"
  putStrLn "End of programm"-}

{-
params :: [String] -> (Integer, Integer)
params [a,b] = (read a, read b)

printQuotients :: Integer -> Integer -> IO ()
printQuotients a b = do
 print $ a `divMod` b
 print $ b `divMod` a

mainAction :: [String] -> IO ()
mainAction args = do
 let (a, b) = params args
 printQuotients a b
  
main = do
 args <- getArgs
 mainAction args `catches` [Handler handleArith,
                            Handler handleArgs,
                            Handler handleOthers]
 putStrLn "END"  
  
handleArith :: ArithException -> IO ()
handleArith _ = putStrLn "Div zerro"

handleArgs :: PatternMatchFail -> IO ()
handleArgs _ = putStrLn "Wrong params"

handleOthers :: SomeException -> IO ()
handleOthers e = putStrLn $ "unnowns error: " ++ show e -}

{-main = do
 (fn:_) <- getArgs
 fileExist <- doesFileExist fn
 if fileExist 
    then do
    contents <- readFile fn
    putStrLn $ "In this file " ++ show (length (lines contents)) ++ " lines."
    else putStrLn "File do not exist." -}

main = do
 (fn:_) <- getArgs
 countLines fn `catch` handler

handler :: IOException -> IO ()
handler e | isDoesNotExistError e =
               case ioeGetFileName e of
                 Just fn -> putStrLn $ "File " ++ fn ++ " does not exist"
                 Nothing -> putStrLn "File does not exist"
          | otherwise = ioError e
 where
  fn = ioeGetFileName e

countLines :: String -> IO ()
countLines fn = do
 contents <- readFile fn
 putStrLn $ "In this file " ++ show (length (lines contents)) ++ " lines."







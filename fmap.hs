import Data.List
import Data.Char

main = do
 a <- (++) <$> getLine <*> getLine
 putStrLn $ "Two lines: " ++ a

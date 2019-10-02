import Data.List
import Data.Char

main = do
 line <- fmap (intersperse '-' . reverse . map toUpper) getLine
 putStrLn line


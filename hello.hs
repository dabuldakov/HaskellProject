module Hello where

class Printable a where
 toString :: a -> [Char]

instance Printable Bool where
 toString True = "true"
 toString False = "false"

instance Printable () where
 toString () = "unit type"

instance (Printable b, Printable c) => Printable (b, c) where
 toString x = "(" ++ (toString $ fst x) ++ "," ++ (toString $ snd x) ++ ")"





  


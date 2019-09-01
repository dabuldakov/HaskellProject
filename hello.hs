class Printable a where
 (True), (False), (()) :: a -> [Char]


instance Printable Bool where
  toSting x = if x == True then "true" else "false"
  
instance Printable () where
  toSting x = "unit type"
{------------------------------------------------------------------------------
 
Type Classes - Defining Set

------------------------------------------------------------------------------}

data Set a = Set [a]

instance Eq a => Eq Set a where
	Set [] == Set [] = True
	Set (x:xs) == Set (y:ys) = x == y && (xs == ys)
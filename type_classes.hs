{------------------------------------------------------------------------------
 
Type Classes - Defining Set

------------------------------------------------------------------------------}

data Set a = Set [a] deriving Show

instance Eq a => Eq (Set a) where
	Set [] == Set [] = True
	Set [] == Set (x:xs) = False
	Set (x:xs) == Set [] = False
	Set (x:xs) == Set (y:ys) = x == y && (xs == ys)
	Set l1 /= Set l2 = not (l1 == l2)

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

instance Ord a => Ord (Set a) where
	compare (Set l1) (Set l2)
		| l1 == l2 = EQ
		| l1 `isSubSetOf` l2 = LT
		| otherwise = GT
	max (Set l1) (Set l2) = Set (union l1 l2)
	min (Set l1) (Set l2) = Set (insersect l1 l2)

isSubSetOf :: Eq a => [a] -> [a] -> Bool
isSubSetOf [] _ = True
isSubSetOf _ [] = False
isSubSetOf (x:xs) l = (elem x l) && (isSubSetOf xs l)

union [] x = x
union x [] = x
union (x:xs) l2
	| elem x l2 = union xs l2
	| otherwise = x : union xs l2

insersect [] _ = []
insersect _ [] = []
insersect (x:xs) l
	| elem x l = x : insersect xs l
	| otherwise = insersect xs l
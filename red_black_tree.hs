{------------------------------------------------------------------------------
	Pretty interesting way to check the power of haskells' pattern matching
	features.
 ------------------------------------------------------------------------------}

data Color = R | B deriving Show
data Tree a = E | T Color (Tree a) a (Tree a) deriving Show

{-----------------------------------------------------------------------------
indicates whether a given value is a member of the red-black tree

	tests:
		member E 1 :: False
		member (T R (E) 2 (E)) 1 :: False
		member (T R (E) 1 (E)) 1 :: True
		member (T R E 1 (T B E 2 E)) 2 :: True
	
------------------------------------------------------------------------------}
member :: Ord a => Tree a -> a -> Bool
member E _ = False
member (T _ t1 v t2) x
		| (v == x) = True
		| (v > x) = member t1 x
		| otherwise = member t2 x
		

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
		
insert t x = turnBlack (ins t x)
	where 
		turnBlack (T _ t1 v t2) = T B t1 v t2
		ins E x = T R E x E
		ins (T c t1 v t2) x = if(x == v) then (T c t1 v t2) 
			else if(x < v) then balance c (ins t1 x) v t2
			else balance c t1 v (ins t2 x)

balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c t1 x t2 = T c t1 x t2 -- caso base

{------------------------------------------------------------------------------
 
data Color = R | B deriving Show
data RB a = E | T Color (RB a) a (RB a) deriving Show

insert :: Ord a => a -> RB a -> RB a
insert x s =
	T B a z b
	where
	T _ a z b = ins s
	ins E = T R E x E
	ins s@(T B a y b)
		| x<y = balance (ins a) y b
		| x>y = balance a y (ins b)
		| otherwise = s
	ins s@(T R a y b)
		| x<y = T R (ins a) y b
		| x>y = T R a y (ins b)
		| otherwise = s

member :: Ord a => a -> RB a -> Bool
member x E = False
member x (T _ a y b)
	| x<y = member x a
	| x>y = member x b
	| otherwise = True

{- balance: first equation is new,
   to make it work with a weaker invariant -}
balance :: RB a -> a -> RB a -> RB a
balance (T R a x b) y (T R c z d) = T R (T B a x b) y (T B c z d)
balance (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance a x b = T B a x b

 ------------------------------------------------------------------------------}

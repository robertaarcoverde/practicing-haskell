{------------------------------------------------------------------------------
 | 1) Redefining Take and Drop
 ------------------------------------------------------------------------------}

{------------------------------------------------------------------------------
	take' function
 ------------------------------------------------------------------------------}

take' 0 xs = []
take' n [] = []
take' n (x:xs) = x : take' (n-1) xs

{------------------------------------------------------------------------------
	drop' function
 ------------------------------------------------------------------------------}

drop' 0 l = l
drop' n [] = []
drop' n (x:xs) = drop' (n - 1) xs

{------------------------------------------------------------------------------
 | 2) Extending Permutations' list-comprehension expressions
 ------------------------------------------------------------------------------}

{------------------------------------------------------------------------------
	original permutations function
 ------------------------------------------------------------------------------}

permutations [] = [[]]
permutations (xs) = [ r | z <- xs, r <- map (z:) (permutations (remove z xs))]

remove :: Eq a => a -> [a] -> [a]
remove x (y:ys) = if (x == y) then ys else y:(remove x ys)

{------------------------------------------------------------------------------
	extended permutations function
 ------------------------------------------------------------------------------}

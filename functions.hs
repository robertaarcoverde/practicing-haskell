{------------------------------------------------------------------------------
 | Functional Programming exercises
 ------------------------------------------------------------------------------}

{------------------------------------------------------------------------------
	concat' function
 ------------------------------------------------------------------------------}

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs
	
-- example: concat' ["abc","de","fg"] = "abcdefg"

{------------------------------------------------------------------------------
	reverse' function
 ------------------------------------------------------------------------------}

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
	
-- example: reverse' "roberta" = "atrebor"


{------------------------------------------------------------------------------
	comb function (combination)
 ------------------------------------------------------------------------------}

comb :: [a] -> Integer -> [[a]]
comb _ 0  = [[]]
comb [] _ = []
comb (x:xs) n = map (x:) (comb xs (n-1)) ++ comb xs n


{------------------------------------------------------------------------------
	powerset function
 ------------------------------------------------------------------------------}

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = ss ++ (map (x:) ss) 
 where ss = powerset xs

-- powerset of a list is the powerset of that same list minus the head, combined with the head in all possible ways
-- for ex, powerset "abc" = powerset "bc" ++ "a" combined with each element in powerset "bc"


{------------------------------------------------------------------------------
	partitions function
 ------------------------------------------------------------------------------}

part :: Integer -> Integer -> [[Integer]]
part n l
 | (n < 0) = []
 | (n == 0) = [[]]
 | otherwise = [ r | l' <- [l, (l-1)..1], r <- map (l':) (part (n - l') l')]


{------------------------------------------------------------------------------
	permutations function
 ------------------------------------------------------------------------------}


permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = [ r | z <- (x:xs), r <- map (z:) (permutations (xs))]

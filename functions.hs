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

-- example: comb [1..4] 2 = [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]


{------------------------------------------------------------------------------
	powerset function
 ------------------------------------------------------------------------------}

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = ss ++ (map (x:) ss) 
 where ss = powerset xs

-- powerset of a list is the powerset of that same list minus the head, combined with the head in all possible ways
-- for ex, powerset "abc" = powerset "bc" ++ "a" combined with each element in powerset "bc"

-- example: powerset "abc" = ["","c","b","bc","a","ac","ab","abc"]

{------------------------------------------------------------------------------
	partitions function
 ------------------------------------------------------------------------------}

part :: Integer -> Integer -> [[Integer]]
part n l
 | (n < 0) = []
 | (n == 0) = [[]]
 | otherwise = [ r | l' <- [l, (l-1)..1], r <- map (l':) (part (n - l') l')]


-- example: part 5 3 = [[3,2],[3,1,1],[2,2,1],[2,1,1,1],[1,1,1,1,1]]


{------------------------------------------------------------------------------
	permutations function
 ------------------------------------------------------------------------------}

-- this depends on an adequate definition of 'remove' to work
permutations [] = [[]]
permutations (xs) = [ r | z <- xs, r <- map (z:) (permutations (remove z xs))]

remove :: Eq a => a -> [a] -> [a]
remove x (y:ys) = if (x == y) then ys else y:(remove x ys)

-- example: permutations [1,2,3] = [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
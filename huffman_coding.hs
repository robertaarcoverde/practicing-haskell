{------------------------------------------------------------------------------
 | The Huffman Coding - http://en.wikipedia.org/wiki/Huffman_coding
 
 This is a Haskell implementation of the Huffman coding algorithm.
 ------------------------------------------------------------------------------}

{------------------------------------------------------------------------------
	We first start defining our data structures: a simple binary tree
	(holding values on the leaves only) and a pair we call Huff, that associates
	a tree of chars with its weight. The BTree needs a value to be constructed;
	a Huff is only an alias for a pair between a BTree and an integer (the
	tree's weight). We could eliminate this and just use the pair everywhere,
	but defining this alias makes the rest of our code cleaner and more
	organized
 ------------------------------------------------------------------------------}

data BTree a = Leaf a | Fork (BTree a) (BTree a)

type Huff = (BTree Char, Int)

type CodeTable = [(Char,[Int])] 

{--- decode function -----------------------

Some test examples:

decode (Fork (Fork (Leaf 'a') (Leaf 'b')) (Leaf 'c')) [1] ==> "c"
decode (Fork (Fork (Leaf 'a') (Leaf 'b')) (Leaf 'c')) [0,1] ==> "b"

--------------------------------------------} 
decode :: BTree Char -> [Int] -> [Char]
decode b [] = []
decode b cs = decode' b cs
	where 
		decode' (Leaf c) cs' = c : decode b cs'
		decode' (Fork t1 t2) (0:cs') = (decode' t1 cs')
		decode' (Fork t1 t2) (1:cs') = (decode' t2 cs')
		
transform :: BTree Char -> CodeTable
transform (Leaf c) = [(c, [])]
transform (Fork t1 t2) = ((ins 0 (transform t1)) ++ (ins 1 (transform t2)))
	where
		ins b = map (\(c,l) -> (c,b:l)) -- inserts the int b in front of each list in the CodeTable pairs

encode :: BTree Char -> [Char] -> [Int]
encode b [] = []
encode b (c:cs) = (lookupChar t c) ++ encode b cs
	where
		t = transform b

lookupChar :: CodeTable -> Char -> [Int]
lookupChar [] _ = []
lookupChar ((a,l):xs) c
	| (a == c) = l
	| otherwise = lookupChar xs c
	
-- now, lets build the tree!

sample :: [Char] -> [(Char, Int)]
sample = sortf freq . collate . sortf (<)

freq :: (Char, Int) -> (Char, Int) -> Bool
freq (_, n1) (_, n2) = n1 < n2

collate :: [Char] -> [(Char, Int)]
collate [] = []
collate (x:xs) = (x, length ys + 1) : collate zs
	where
		(ys, zs) = span (==x) xs

--span p xs = (takewhile p xs, dropwhile p xs) -- essa funcao ja existia no Prelude

-- implementing quick sort, just because its cool
sortf :: Ord a => (a -> a -> Bool) -> [a] -> [a]
sortf f [] = []
sortf f (x:xs) = sortf (f) (filter (not . f x) xs)  ++ [x] ++ sortf f (filter (f x) xs)
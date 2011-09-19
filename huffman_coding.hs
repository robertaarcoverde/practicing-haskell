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
	deriving Show
	
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
		(ys, zs) = span' (==x) xs

span' p xs = (takeWhile p xs, dropWhile p xs)

-- finally, lets connect the dots!

mkHuff :: [(Char, Int)] -> Huff
mkHuff = head . until' singleton combine . map cHuff
	where
		cHuff (c, n) = (Leaf c, n)

-- this is probably my favorite function evah
-- Prelude already holds a definition for until,
-- so lets call this one until'
until' :: (a -> Bool) -> (a -> a) -> a -> a
until' p f a
	| (p a) = a
	| otherwise = until' p f (f a)

singleton :: [a] -> Bool
singleton xs
	| length xs == 1 = True
	| otherwise = False
	
-- combines the first two huffs into one
-- and inserts it into the right index
combine :: [Huff] -> [Huff]
combine (x:y:xs) = sortf (huffWeight) (merge x y : xs)
	where
		merge (a, n1) (b, n2) = ((Fork a b), (n1 + n2))

huffWeight :: Huff -> Huff -> Bool
huffWeight (b, n1) (c, n2) = n1 < n2
		
-- implementing quick sort, just because its cool
sortf :: (a -> a -> Bool) -> [a] -> [a]
sortf f [] = []
sortf f (x:xs) = sortf (f) (filter (not . f x) xs)  ++ [x] ++ sortf f (filter (f x) xs)
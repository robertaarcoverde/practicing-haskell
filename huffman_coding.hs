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
 

 
decode :: BTree Char -> [Int] -> [Char]
decode b [] = []
decode b cs = decode' b cs
	where 
		decode' (Leaf c) cs' = c : decode b cs'
		decode' (Fork t1 t2) (0:cs') = (decode' t1 cs')
		decode' (Fork t1 t2) (1:cs') = (decode' t2 cs')
		

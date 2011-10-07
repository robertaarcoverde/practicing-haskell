{------------------------------------------------------------------------------
 
Leftist Heap

------------------------------------------------------------------------------}

data Heap a = E | T Int a (Heap a) (Heap a) deriving Show

merge :: Ord a => Heap a -> Heap a -> Heap a
merge E t = t
merge t E = t
merge t1@(T r1 v1 a1 b1) t2@(T r2 v2 a2 b2)
	| (v1 < v2) = makeT v1 a1 (merge b1 t2)
	| otherwise = makeT v2 a2 (merge b2 t1)
	where
		makeT v t1 t2 = if rank t1 >= rank t2 then T (rank t2 + 1) v t1 t2
			else T (rank t1 + 1) v t2 t1

rank E = 0
rank (T r v1 a1 b1) = r

insert :: Ord a => a -> Heap a -> Heap a
insert v h = merge (T 1 v E E) h

delete :: Ord a => Heap a -> Heap a
delete E = E
delete (T _ v1 h1 h2) = merge h1 h2

sort :: Ord a => [a] -> [a]
sort [] = []
sort l = makeList (makeHeap l)

makeHeap [] = E
makeHeap (x:xs) = merge (T 1 x E E) (makeHeap xs)

makeList E = []
makeList t@(T r v h1 h2) = v : makeList(delete t)
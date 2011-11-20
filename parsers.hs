-- monad
newtype Parser a = MkP (String -> [(a,String)])

apply :: Parser a -> String -> [(a,String)]
apply (MkP f) s = f s

instance Monad Parser where
    return x    = MkP f where f s = [(x,s)]
    p >>= q     = MkP f
        where f s = [(y,s'') | (x,s') <- apply p s, (y,s'') <- apply (q x) s']

getchar :: Parser Char
getchar = MkP f
    where f (c:s) = [(c,s)]
          f [] = []		
		  
condchar :: (Char -> Bool) -> Parser Char
condchar f = do { c <- getchar ; if (f c) then return c else fail "condition" }

checkchar c = condchar (==c)

plus :: Parser a -> Parser a -> Parser a
plus p1 p2 = MkP f
    where f s = (apply p1 s) ++ (apply p2 s)

orelse :: Parser a -> Parser a -> Parser a
orelse p1 p2 = MkP f
    where f s = let l = apply p1 s in
                    if null l then apply p2 s
                              else l

data Exp = ExpChar Char
    | ExpOr Exp Exp        
    | ExpPlus Exp
    | ExpSet (Char -> Bool)
    | ExpSeq Exp Exp
    | ExpDot							  
	
	
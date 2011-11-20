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
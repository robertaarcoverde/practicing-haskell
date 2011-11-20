-- monad
newtype Parser a = MkP (String -> [(a,String)])

apply :: Parser a -> String -> [(a,String)]
apply (MkP f) s = f s

instance Monad Parser where
    return x    = MkP f where f s = [(x,s)]
    p >>= q     = MkP f
        where f s = [(y,s'') | (x,s') <- apply p s, (y,s'') <- apply (q x) s']

		
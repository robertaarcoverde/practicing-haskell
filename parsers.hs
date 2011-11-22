{----------------------------------------------
	defining the parser monad
-----------------------------------------------}
newtype Parser a = MkP (String -> [(a,String)])

apply :: Parser a -> String -> [(a,String)]
apply (MkP f) s = f s

instance Monad Parser where
    return x    = MkP f where f s = [(x,s)]
    p >>= q     = MkP f
        where f s = [(y,s'') | (x,s') <- apply p s, (y,s'') <- apply (q x) s']

parser :: Parser a -> String -> a
parser p s = fst (head (apply p s))
		
{----------------------------------------------
	chars
-----------------------------------------------}	
getchar :: Parser Char
getchar = MkP f
    where f (c:s) = [(c,s)]
          f [] = []		
		  
condchar :: (Char -> Bool) -> Parser Char
condchar f = do { c <- getchar ; if (f c) then return c else fail ("condition failed for char " ++ [c]) }

checkchar c = condchar (==c)

char = condchar (not . (flip elem "*?.+[]()|"))

-- testing:
-- apply (checkchar '*') "*" --> [('*',"")]
-- apply (checkchar '*') "a" --> *** Exception: could not match char a
-- apply (checkchar '*') "*asd" --> [('*',"asd")]

{----------------------------------------------
	manipulating parsers
-----------------------------------------------}
plus :: Parser a -> Parser a -> Parser a
plus p1 p2 = MkP f
    where f s = (apply p1 s) ++ (apply p2 s)

orelse :: Parser a -> Parser a -> Parser a
orelse p1 p2 = MkP f
    where f s = let l = apply p1 s in
                    if null l then apply p2 s
                              else l							  

rep :: Parser a -> Parser [a]
rep p = rep1 p `orelse` return []

rep1 :: Parser a -> Parser [a]
rep1 p = do { x <- p ;  xs <- rep p;  return (x:xs)}							  
							  
{----------------------------------------------
	cool, generic functions
-----------------------------------------------}	
-- isso nao funcionou:
---- convertp f p = do { x <- p; return (f x) }
---- concatp = convertp concat
---- pc2ps = convertp (:[])
-- entao fiz assim mesmo:
concatp p = do { l <- p; return (concat l)}
pc2ps p = do { l <- p; return [l] }

{----------------------------------------------
	regexp parser (per se)
-----------------------------------------------}	
-- exp ::=  exp '|' seq  
--          |  seq


-- seq ::=  seq suffixed  
--          |  suffixed


-- suffixed ::= primary
--          | primary *
--          | primary ?
--          | primary +
suffixed :: Parser (Parser String)
suffixed = do {
	s <- primary;
	f <- suffix;
	return (f s);
	}

suffix :: Parser (Parser String -> Parser String)
suffix = do {
	checkchar '*';
	return (concatp rep);
	}

-- primary ::= char
--      | '(' exp ')'
--      | '[' charset ']'
--      | '.'
primary :: Parser (Parser String)
primary = do {
	checkchar '.';
	return (pc2ps getchar);
	}

-- charset ::= '^'? [^]]*




{----------------------------------------------
	tests
-----------------------------------------------}

-- digitos, email, url, cpf, data...
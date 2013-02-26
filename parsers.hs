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

zero :: Parser a
zero = MkP (\p -> [])
		
{----------------------------------------------
	chars
-----------------------------------------------}	
getchar :: Parser Char
getchar = MkP f
    where f (c:s) = [(c,s)]
          f [] = []		
		  
condchar :: (Char -> Bool) -> Parser Char
condchar f = do { c <- getchar ; if (f c) then return c else zero }

checkchar c = condchar (==c)

char = condchar (not . (flip elem "*?.+[]()|"))

escaped = do { condchar (=='\\') ; c <- getchar ; return c }

-- testing:
-- apply (checkchar '*') "*" --> [('*',"")]
-- apply (checkchar '*') "a" --> []
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

opt :: Parser String -> Parser String
opt p = p `orelse` pEmpty

pEmpty :: Parser String
pEmpty = ( return "" )				  
												
{----------------------------------------------
	cool, generic functions
-----------------------------------------------}	
convertp f p = do { x <- p; return (f x) }

concatp = convertp concat

pc2ps = convertp (:[])

{----------------------------------------------
	parser (per se)
-----------------------------------------------}	
-- exp ::=  exp '|' seq  
--          |  seq
-- seq ::=  seq suffixed  
--          |  suffixed
-- suffixed ::= primary
--          | primary *
--          | primary ?
--          | primary +
-- primary ::= char
--      | '(' exp ')'
--      | '[' charset ']'
--      | '.'
-- charset ::= '^'? [^]]* -- negacao opcional seguido de qqr coisa menos ']' 0 ou mais vezes

exp' :: Parser (Parser String)
exp' = do
    x <- seq'
    xs <- rep (do {checkchar '|'; seq'}) 
    return (foldr1 plus (x:xs))

seq' :: Parser (Parser String)
seq' = do { l <- rep1 suffixed ; return (concatp (sequence l)) } 

suffixed :: Parser (Parser String)
suffixed = do {
	s <- primary;
	f <- suffix;
	return (f s);
	}

suffix :: Parser (Parser String -> Parser String)
suffix = do {
		checkchar '*';
		return (concatp . rep)
	} `orelse` do { 
		checkchar '?';
		return opt
	} `orelse` do {
		checkchar '+';
		return (concatp . rep1)
	} `orelse` do {
		return id
	}

primary :: Parser (Parser String)
primary = do {
		checkchar '.';
		return (pc2ps getchar);
	} `orelse` do {
		checkchar '(';
		e <- exp';
		checkchar ')';
		return e;
	} `orelse` do {
		checkchar '[';
		l <- rep classitem;
		checkchar ']';
		return (pc2ps (f l));
	} `orelse` do {
		c <- escaped `orelse` char;
		return (pc2ps (checkchar c));
	}
	
f l = condchar (\c -> or (map (flip ($) c) l))	

classitem :: Parser( Char -> Bool )
classitem = do {
		ci <- (range `orelse` charmatches);
		return ci;
	}
	
range :: Parser (Char -> Bool)
range = do
    from <- condchar  (/=']');
    checkchar '-';
    to <- condchar (/=']');
    return (`elem` [from..to]);

charmatches :: Parser (Char -> Bool)
charmatches = do { c <- condchar (/=']'); return (==c) }

{----------------------------------------------
	tests
-----------------------------------------------}
test :: String -> String -> [(String,String)]
test re str = apply (parser exp' re) str  

test_seq = (if test "abc" "abc" == [("abc", "")] then "Passed" else "Failed") ++ " (test 'abc' 'abc')\n"
test_star = (if test "a*" "aaa" == [("aaa", "")] then "Passed" else "Failed") ++ " (test 'a*' 'aaa')\n"
test_opt = (if test "a?b" "ab" == [("ab", "")] then "Passed" else "Failed") ++ " (test 'a?b' 'ab')\n"
test_opt2 = (if test "a?b" "b" == [("b", "")] then "Passed" else "Failed") ++ " (test 'a?b' 'b')\n"
test_classitem = (if test "[Rr]oberta" "roberta" == [("roberta", "")] then "Passed" else "Failed") ++ " (test '[Rr]oberta' 'roberta')\n"
test_classitem2 = (if test "[Rr]oberta" "Roberta" == [("Roberta", "")] then "Passed" else "Failed") ++ " (test '[Rr]oberta' 'Roberta')\n"
test_range = (if test "[0-9]+" "12345" == [("12345", "")] then "Passed" else "Failed") ++ " (test '[0-9]+' '12345')\n"

test_url = (if test "((https?|ftp|telnet|file)://[[a-zA-Z0-9:#@%/;$()~_?\\+-=.&]*)" "http://www.google.com" == [("http://www.google.com", "")] then "Passed" else "Failed") ++ " (test url)\n"
test_email = (if test "[A-Za-z0-9._-]+@[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+(\\.[A-Za-z0-9_-]+)?" "rarcoverde@inf.puc.br" == [("rarcoverde@inf.puc.br", "")] then "Passed" else "Failed") ++ " (test email)\n"

test_everything = putStr ( test_seq ++ test_star ++ test_opt ++ test_opt2 ++ test_classitem ++ test_classitem2 ++ test_range ++ test_url ++ test_email )

-- digitos, email, url, cpf, data...
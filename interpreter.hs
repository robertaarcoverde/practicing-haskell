{------------------------------------------------------------------------------
 Mini-Interpreter
------------------------------------------------------------------------------}

data Exp = ExpK Int
	| ExpVar String
	| ExpAsg String Exp
	| ExpAdd Exp Exp
	| ExpSeq Exp Exp
	| ExpIf Exp Exp Exp
	
type S = String -> Int
	
update :: S -> String -> Int -> S
update f var v = f
	
eval :: Exp -> S -> (Int, S)
eval (ExpK n) s = (n, s)
eval (ExpVar var) s = (s var, s)
eval (ExpAsg var e) s = (v, update s' var v)
	where
		(v, s') = eval e s
		
eval (ExpSeq e1 e2) s = eval e2 s'
	where
		(_, s') = eval e1 s

eval (ExpIf e1 e2 e3) s = if (c /= 0) then eval e2 s' else eval e3 s'
	where
		(c, s') = eval e1 s

eval (ExpAdd e1 e2) s = ((v1 + v2), s'')
	where 
		(v1, s') = eval e1 s
		(v2, s'') = eval e2 s'

{------------------------------------------------------------------------------
 Mini-Interpreter
------------------------------------------------------------------------------}

data Exp = ExpK Int
	| ExpVar String
	| ExpAsg String Exp
	| ExpAdd Exp Exp
	| ExpSeq Exp Exp
	| ExpIf Exp Exp Exp  deriving Show
	
type S = String -> Int

-- initializing everyone with 0
initstore :: S
initstore id = 0
-- updating the store returns a new store (representing the new environment)
update :: S -> String -> Int -> S
update s var v = s'
	where
		s' var'
			| var' == var = v
			| otherwise = s var'
	
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

-- para testar:
to_s :: (Int, S) -> String
to_s (v, _) = show v

-- Examples:

-- a = 1 -->> (ExpAsg "a" (ExpK 1))
-- b = 2 -->> (ExpAsg "b" (ExpK 2))
-- add a b --> ExpSeq (ExpSeq (ExpAsg "a" (ExpK 1)) (ExpAsg "b" (ExpK 2))) (ExpAdd (ExpVar "a") (ExpVar "b"))
test_a_plus_b_equals_three :: String
test_a_plus_b_equals_three
	| to_s (eval (ExpSeq (ExpSeq (ExpAsg "a" (ExpK 1)) (ExpAsg "b" (ExpK 2))) (ExpAdd (ExpVar "a") (ExpVar "b"))) initstore) == "3" = "Passed"
	| otherwise = "Failed - a + b should be 3"

-- a = 1 -->> (ExpAsg "a" (ExpK 1))
-- b = 2 -->> (ExpAsg "b" (ExpK 2))
-- if (a) then a = 3 else a = 2 (ExpIf (ExpVar "a") (ExpAsg "a" (ExpK 3)) (ExpAsg "a" (ExpK 2)))
-- add a b
test_if_then_else :: String
test_if_then_else
	| to_s (eval 
			(ExpSeq 
				(ExpSeq 
					(ExpSeq 
						(ExpAsg "a" (ExpK 1))  -- a = 1
						(ExpAsg "b" (ExpK 2))) -- b = 2
					(ExpIf (ExpVar "a")        -- if (a)
						(ExpAsg "a" (ExpK 3))  -- then a = 3
						(ExpAsg "a" (ExpK 2)))) -- else a = 2
					(ExpAdd (ExpVar "a") (ExpVar "b"))) -- add a b
				initstore) == "5" = "Passed" -- passes if a + b = 5
	| otherwise = "Failed - a + b should be 3" -- fails otherwise


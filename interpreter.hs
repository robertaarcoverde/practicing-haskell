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

newtype SE a = SE { makeSE :: S -> (a, S)}

instance Monad SE where
	return x = ret x
	s >>= f = seqv s f

-- return function
ret :: a -> SE a
ret x = SE (\s -> (x, s))

-- >>= function	
seqv :: SE a -> (a -> SE b) -> SE b
seqv se1 f = SE (\se2 -> let (v, s') = makeSE se1 se2 in makeSE (f v) s')

seq' :: SE a -> SE b -> SE b
seq' se1 se2 = seqv se1 (\_ -> se2)

-- funcao auxiliar para extrair a funcao do SE
-- (nao sei se ha um modo mais elegante de fazer isso.. deve haver)
extSE :: SE a -> (S -> (a, S))
extSE (SE p) = p

-- inicializar o store
initstore :: S
initstore id = 0

-- atualizar o store retorna um novo store
update :: S -> String -> Int -> S
update s var v = s'
	where
		s' var'
			| var' == var = v
			| otherwise = s var'
	
-- avaliando as expressoes
eval :: Exp -> S -> (Int, S)
eval (ExpK n) = extSE (ret n)
eval (ExpVar var) = \s -> (s var, s)
eval (ExpAsg var e) = \s -> let (v, s') = eval e s in (v, update s' var v)	
		
eval (ExpSeq e1 e2) = extSE (seq' (SE (eval e1)) (SE(eval e2)))
eval (ExpIf e1 e2 e3) = extSE (seqv (SE (eval e1)) (\b -> if (b /= 0) then SE (eval e2) else SE (eval e3)))
eval (ExpAdd se1 se2) = extSE (seqv (SE (eval se1)) (\v1 -> seqv (SE (eval se2)) (\v2 -> ret (v1 + v2))))

-- funcao auxiliar de testes
to_s :: (Int, S) -> String
to_s (v, _) = show v

-- Examples:

-- a = 1 -->> (ExpAsg "a" (ExpK 1))
-- b = 2 -->> (ExpAsg "b" (ExpK 2))
-- add a b -->> ExpSeq (ExpSeq (ExpAsg "a" (ExpK 1)) (ExpAsg "b" (ExpK 2))) (ExpAdd (ExpVar "a") (ExpVar "b"))
test_a_plus_b_equals_three :: String
test_a_plus_b_equals_three
	| to_s (eval (ExpSeq (ExpSeq (ExpAsg "a" (ExpK 1)) (ExpAsg "b" (ExpK 2))) (ExpAdd (ExpVar "a") (ExpVar "b"))) initstore) == "3" = "Passed"
	| otherwise = "Failed - a + b should be 3"

-- a = 1 -->> (ExpAsg "a" (ExpK 1))
-- b = 2 -->> (ExpAsg "b" (ExpK 2))
-- if (a) then a = 3 else a = 2 -->> (ExpIf (ExpVar "a") (ExpAsg "a" (ExpK 3)) (ExpAsg "a" (ExpK 2)))
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


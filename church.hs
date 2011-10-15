{------------------------------------------------------------------------------
 
Church Numerals

------------------------------------------------------------------------------}
data N = Z | S N

type Nat = (b -> b) -> b -> b

zero = \f z -> z
churchSucc x = \f z -> f (x f z)

toInt :: Nat -> Int
toInt n = n (+1) 0

isZero :: Nat -> Bool
isZero n = n (\_ -> False) True
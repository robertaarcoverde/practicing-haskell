{------------------------------------------------------------------------------
	Pretty interesting way to check the power of haskells' pattern matching
	features.
 ------------------------------------------------------------------------------}

 data Color = R | B
 data Tree a = E | T Color (Tree a) a (Tree a)
{------------------------------------------------------------------------------
 | 1) Redefining Take and Drop
	
	checking what happens when applying regular take and drop
	to partial lists	
	
	take 0 undefined = []
	take 2 undefined = Exception! (undefined) //oops!
	take undefined [] = Exception! (undefined) //oops!
	take undefined [1..3] = Exception! (undefined) //oops!

	the first test worked properly because of the definition of take:		
		take 0 xs = []
		take n [] = []
		take n (x:xs) = x : take' (n-1) xs
	
	it matches n, and returns an empty list, regardless of xs. the third test,
	however, does not work:
		take undefined [] = undefined!
	
	that happens because, when evaluating the first expression, it tries to
	match undefined with 0 - and this gives us bottom (or undefined)!
	
	now, lets check what happens when we redefe take, changing the order of 
	the 2 first expressions:
 ------------------------------------------------------------------------------}

take' :: Integer -> [a] -> [a]
take' n [] = []
take' 0 xs = []
take' n (x:xs) = x : take' (n-1) xs

{------------------------------------------------------------------------------

	take' 0 undefined = Exception! (undefined)
	take' 2 undefined = Exception! (undefined)
	take' undefined [] = []
	take' undefined [1..3] = Exception! (undefined)
	
	...and we got some different results here! since now take 0 xs is the
	second expression, when we apply the first test (take' 0 undefined),
	it will try to match 0 against n and undefined against the empty list - and
	that results in undefined! however, when testing (take' undefined []), 
	the function correctly returns the empty list. we can't have both cases
	working on the same implementation. same thing happens with drop:

 ------------------------------------------------------------------------------}

drop' n [] = []
drop' 0 l = l
drop' n (x:xs) = drop' (n - 1) xs

 {------------------------------------------------------------------------------	
	drop 0 undefined = Exception! (undefined)
	drop 2 undefined = Exception! (undefined)
	drop undefined [] = Exception! (undefined)
	drop undefined [1..3] = Exception! (undefined)
	
	drop' 0 undefined = Exception! (undefined)
	drop' 2 undefined = Exception! (undefined)
	drop' undefined [] = []
	drop' undefined [1..3] = Exception! (undefined)
	
------------------------------------------------------------------------------}
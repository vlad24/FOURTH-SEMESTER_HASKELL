--accumulator usage
validate :: String -> Bool
validate (x : xs)  = bracketsOK (x:xs) 0 where
	bracketsOK :: String -> Int -> Bool
	bracketsOK [] balance = (balance == 0)
	bracketsOK (x : xs) balance | ( x == '(' )        =  if (balance + 1 >= 0) then  bracketsOK (xs) (balance + 1)
						 else False
			            | ( x == ')' )        =  if (balance - 1 >= 0) then  bracketsOK (xs) (balance - 1)
						 else False
			            | otherwise         =  bracketsOK (xs) balance
main = do
	putStrLn("Enter your string please and we will check the right sequence of brackets there: ")
	putStr("Your string : ")
	str <- getLine
	if (validate str) then putStrLn("Right sequence")
		         else putStrLn("Wrong sequence")

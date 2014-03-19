isClosingPair :: Char -> Char -> Bool
isClosingPair '(' ')' = True
isClosingPair '[' ']' = True
isClosingPair '{' '}' = True
isClosingPair _ _ = False

isOpen :: Char -> Bool
isOpen x  = elem x  ['(', '[', '{']

isClose :: Char -> Bool
isClose x = elem x [')', ']', '}']

validate :: String -> Bool
validate (x : xs)  = bracketsOK (x:xs) [] where
	bracketsOK :: String->[Char]->Bool
	bracketsOK [] [] = True
	bracketsOK [] stack = False
	bracketsOK (x:xs) stack | (isOpen x) = bracketsOK xs (x:stack)
			       | (isClose x) = if (isClosingPair (head stack) x) 
					     then bracketsOK xs (tail stack)
				                  else False
			       | otherwise = bracketsOK xs stack
main = do
	putStrLn("Enter your string please and we will check the right sequence of brackets there: ")
	putStr("Your string : ")
	str <- getLine
	if (validate str) then putStrLn("Right sequence")
		         else putStrLn("Wrong sequence")

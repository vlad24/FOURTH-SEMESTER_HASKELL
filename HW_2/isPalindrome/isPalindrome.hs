isPalindrome :: String -> Bool
isPalindrome string = equals string  (reverse string) where
	equals [] [] = True
	equals [] _ = False
	equals _ [] = True
	equals (face1:body1)  (face2:body2) = if (face1 == face2)
					then equals body1 body2
					else False

main :: IO ()
main = do
	putStr("Enter your string. We will figure out whether is is a palindrome or not: ")
	input <- getLine
	print("It is " ++  show(isPalindrome(input)) ++ " that your string is palindrome")
	
--fibNumbers
fibNumbers :: Integer -> Integer
fibNumbers 0 = 0
fibNumbers 1 = 1
fibNumbers n | n < 0 = error "Negative indexes for fib numbers are not avaliable yet, sir"
		  | otherwise = fibNumbers(n - 1) + fibNumbers(n - 2)

main = do
	putStrLn("Enter the number of fib number you would like to find")
	n <- readLn
	print (fibNumbers(n))
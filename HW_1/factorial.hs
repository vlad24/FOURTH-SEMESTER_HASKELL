factorialByLists :: Integer -> Integer
factorialByLists 0 = 1
factorialByLists n | n > 0 = product [1..n] 
		   | otherwise = error "Sorry, we cant take integrals yet, only factorials with natural numbers and zeros are avaliable"
		
factorialBasic :: Integer -> Integer
factorialBasic 0 = 1
factorialBasic n = if n < 0
		   then error  "Sorry, we cant take integrals yet, only factorials with natural numbers and zeros are avaliable"
		   else n * factorialBasic(n - 1)
main = do
	 putStrLn("Enter a non-negative integer number, factorial of which you would like to calculate : ")
	 n <- readLn
	 print(factorialByLists n)
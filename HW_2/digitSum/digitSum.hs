sumDigits :: Int -> Int
sumDigits number = sumLastDigit (abs number) 0  where
                sumLastDigit :: Int -> Int -> Int
                sumLastDigit number sum = if (number < 10)
                                                               then number + sum
                                                               else sumLastDigit (div number 10) (sum + mod  number 10)
--
main :: IO()
main = do
	putStr("Enter an integer number, digit sum of which you would like to count : ")
	n <- readLn :: IO Int
	putStr("Here is your sum: ")
	print(sumDigits(n))

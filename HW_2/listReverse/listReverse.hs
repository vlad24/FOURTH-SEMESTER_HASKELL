-- reverseList - a funcion that reverses a list.

reverseList :: [Int] -> [Int]
reverseList (first:remainder) = subReverse (first:remainder) []
	where
		subReverse :: [Int] -> [Int] -> [Int]
		subReverse [] inverted = inverted
		subReverse (first:remainder) inverted  = subReverse remainder (first:inverted)

main :: IO()
main = do
	putStrLn("Enter your list : ")
	list <- readLn :: IO [Int]
	putStrLn("Your list reversed : ")
	print(reverseList(list))
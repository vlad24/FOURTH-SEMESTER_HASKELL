base = [1,7,9]
list = 1 : 7 : 9 : [10 * firstElement + anotherElement | firstElement <- list, anotherElement <- base]

show179ToUser n = print(take n list)

main :: IO()
main = do
	putStrLn("Enter the n and we will show you the n elements of 1,7,9 list")
	putStr("n = ")
	n <- readLn :: IO Int
	show179ToUser n
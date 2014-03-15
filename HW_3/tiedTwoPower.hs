twoPower = 1 : map(*2) twoPower
showToPowerTwoUser :: Int -> IO()
showToPowerTwoUser n  | n > 0  = print(take n twoPower)
		        | otherwise = error "Negative degrees are not avaliable"
main :: IO()
main = do
	putStrLn("Enter the n and the list of n twoPowers will be generated")
	putStr("n = ")
	n <- readLn :: IO Int
	showToPowerTwoUser n

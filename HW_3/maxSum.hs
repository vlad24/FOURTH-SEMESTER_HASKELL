--Accumulators usage
getMaxPosition :: [Int] -> Int
getMaxPosition list = maxPairSeek list 1 (-32000) (-32000) where
maxPairSeek :: [Int] -> Int -> Int -> Int -> Int
maxPairSeek []  _  _  _ = -32000
maxPairSeek (x:[]) _ returnPosition _ = returnPosition
maxPairSeek (x:(y:xs)) currentPosition returnPosition currentMax = if ((x + y) > currentMax)
							then maxPairSeek (y : xs) (currentPosition + 1) currentPosition (x + y)
							else maxPairSeek (y : xs) (currentPosition + 1) returnPosition currentMax
main :: IO()
main = do
	putStrLn("Enter the list and we will find two max neighbours")
	putStr("list = ")
	list <- readLn :: IO [Int]
	print(getMaxPosition list)
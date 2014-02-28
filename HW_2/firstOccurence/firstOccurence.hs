firstOccurencePosition :: Int -> [Int] -> Int
firstOccurencePosition element list = checkNthHead element list 1 where
	checkNthHead :: Int -> [Int] -> Int -> Int
	checkNthHead element []  n = error("No element found")
	checkNthHead element (first : remainder)  n  =  if (first == element)
				          	               then n
					               else checkNthHead element remainder (n + 1)
--
main  ::  IO ()
main = do
	putStr("Enter your list : ")
	list <- readLn :: IO [Int]
	putStr("Enter the element, which you would like to find  : ")
	element <- readLn :: IO Int
	print("Your element is at " ++  show(firstOccurencePosition element list) ++ " position in the list")


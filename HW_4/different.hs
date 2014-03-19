isDifferent :: Eq a => [a] -> Bool
isDifferent [] = True
isDifferent (x:remainder) | x `elem` remainder = False
	                      | otherwise = isDifferent remainder

main = do
	putStrLn("Let's see whether [2, 4, 1, 4]  consists from different elements :")
	let list  = [2, 4, 1, 4]
	if (isDifferent list) then putStrLn("Yes") else putStrLn("No")
	
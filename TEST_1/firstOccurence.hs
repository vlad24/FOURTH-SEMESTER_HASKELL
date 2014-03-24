--Searches for the first pos in the list of x

findFirstPosition element list = printListIndex $ firstPos element list

printListIndex pos = if (pos < 0) then error "No element" else putStrLn("Your element is on " ++ show(pos))

firstPos x list = (- complexFirstPos x list)

complexFirstPos x list = foldl (\pos  element -> analyze pos element) 0 list where 
				                                               analyze pos element  | pos < 0 = pos
						                                                   | (x == element) = (- (pos + 1))
						                                                   | otherwise  = pos + 1
									-- if we get the element neg the pos and dont care afterwards
									-- if we are standing not on the element ignore it and continue increasing the pos
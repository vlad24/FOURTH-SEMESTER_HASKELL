data Tree t = Leaf t | Node (Tree t) t (Tree t)

superCondition :: Float -> Bool
superCondition x = (x + tan x)^2 < 10 * x

searchInTree :: (t -> Bool) -> Tree t -> [t]
searchInTree condition tree = formList condition [] tree
	where
	formList :: (t -> Bool) -> [t] -> Tree t -> [t]
	formList condition list (Leaf t) | (condition t) = (t : list)
				  | otherwise = list
	formList condition list (Node treeLeft t treeRight) | (condition t) = formList condition (formList condition (t:list) treeLeft) treeRight
						  | otherwise = formList condition (formList condition list treeLeft) treeRight

main = do
	let tree = Node (Node (Leaf 2) 55 (Leaf 3)) 24 (Leaf 576)
	putStrLn "Printing all elements (x + tan x)^2 < 10 * x"
	print(searchInTree superCondition tree)
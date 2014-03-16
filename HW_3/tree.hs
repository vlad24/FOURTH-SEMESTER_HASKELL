data Tree t =  Nil
	       | Root (TreeElement t)

data TreeElement t =  Leaf t 
	                  | TreeNode (TreeElement t) t (TreeElement t)

findMaxHeight :: Tree t -> Int
findMaxHeight Nil = 0
findMaxHeight (Root (wholeTree)) = longestWayIn wholeTree

findMinHeight :: Tree t -> Int
findMinHeight Nil = 0
findMinHeight (Root (wholeTree)) = shortestWayIn wholeTree

longestWayIn :: TreeElement t -> Int
longestWayIn (Leaf t) = 0
longestWayIn (TreeNode(treeElementLeft) t (treeElementRight)) = 1 + max (longestWayIn treeElementLeft) (longestWayIn treeElementRight)

shortestWayIn :: TreeElement t -> Int
shortestWayIn (Leaf t) = 0
shortestWayIn (TreeNode (treeElementLeft) t (treeElementRight)) = 1 + min (shortestWayIn treeElementLeft) (shortestWayIn treeElementRight)

main = do
	let rootSystem = TreeNode (TreeNode (Leaf 1) 2 (Leaf 1)) 4 (Leaf 1)
	let myTree = Root(rootSystem)
	putStrLn "Height : "
	print(findMaxHeight myTree)
	putStrLn "Minimum length of a way to a leaf : "
	print(findMinHeight myTree)
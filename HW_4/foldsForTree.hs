data Tree t = Leaf t | Node (Tree t) t (Tree t)

foldlTree :: (a -> t -> a) -> a -> (Tree t)   ->   a
foldlTree f accumulator (Leaf t) = f accumulator t
foldlTree f accumulator (Node treeLeft t treeRight) = foldlTree f (f (foldlTree f accumulator treeLeft) t) treeRight

foldrTree :: (t -> a -> a) -> (Tree t) -> a  ->   a
foldrTree f (Leaf t)  accumulator = f t accumulator
foldrTree f (Node treeLeft t treeRight) accumulator = foldrTree f treeLeft (f  t (foldrTree f treeRight accumulator ))

main = do
	let tree = Node (Node (Leaf 1) 2 (Leaf 1)) 4 (Leaf 1)
	putStrLn "Left folding 0 with the tree by + : "
	print(foldlTree (+) 0 tree)
	putStrLn "Right folding the tree with 0 by - : "
	print(foldrTree (-) tree 0)
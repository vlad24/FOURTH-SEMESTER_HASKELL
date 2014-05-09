data Tree a = EmptyTree | Node (Tree a) a (Tree a)

printTree :: Show a => Tree a -> IO()

printTree EmptyTree = return()
printTree (Node leftSubtree value rightSubtree) = do
                                                                         putStr ( show $ value)
                                                                         putStr ( " " )
                                                                         printTree leftSubtree
                                                                         printTree rightSubtree
insertIntoTree :: Ord a => Tree a -> a -> Tree a
insertIntoTree (EmptyTree)  value = (Node (EmptyTree) value (EmptyTree))
insertIntoTree (Node treeLeft a treeRight) value | (value  < a) =  (Node (insertIntoTree(treeLeft) value) a treeRight)
                                                                   | (value  > a) =  (Node treeLeft a (insertIntoTree(treeRight) value))
                                                                   | otherwise = (Node treeLeft a treeRight)

isLeaf (Node EmptyTree a EmptyTree) = True
isLeaf _ = False

hasOnlyLeftChild (Node EmptyTree a EmptyTree) = False
hasOnlyLeftChild (Node _ a EmptyTree) = True
hasOnlyLeftChild _ = False

hasOnlyRightChild (Node EmptyTree a EmptyTree) = False
hasOnlyRightChild (Node EmptyTree a _) = True
hasOnlyRightChild _ = False


removeFromTree :: Ord a => Tree a -> a -> Tree a
removeFromTree EmptyTree value = EmptyTree
removeFromTree (Node treeLeft a treeRight) value | (value > a) = (Node treeLeft a (removeFromTree treeRight value))
                                                                       | (value < a) = (Node (removeFromTree treeLeft value) a treeRight)
                                                                       | (value == a) = removeNode (Node treeLeft a treeRight)



removeNode :: Tree a -> Tree a
removeNode (Node treeLeft a treeRight) | isLeaf (Node treeLeft a treeRight) = EmptyTree
                                                         | hasOnlyLeftChild (Node treeLeft a treeRight) = treeLeft
                                                         | hasOnlyRightChild (Node treeLeft a treeRight) = treeRight
                                                         | otherwise = (Node treeLeft (leftest treeRight) (popLeftest treeRight))
                                                                             where
                                                                                     leftest (Node EmptyTree value rightTree) = value
                                                                                     leftest (Node treeLeft value treeRight) = leftest treeLeft
                                                                                     popLeftest (Node EmptyTree value treeRight) = EmptyTree
                                                                                     popLeftest (Node treeLeft value treeRight) = (Node (popLeftest treeLeft) value treeRight)

treeSize :: Tree a -> Int
treeSize EmptyTree = 0
treeSize (Node treeLeft a treeRight) = 1 + treeSize(treeLeft) + treeSize(treeRight)


treeHeight :: Tree a -> Int
treeHeight EmptyTree = 0
treeHeight (Node treeLeft a treeRight) = 1 + max  (treeHeight treeLeft) (treeHeight treeRight)

main = do
let t = Node (Node EmptyTree 0 (Node EmptyTree 2 EmptyTree)) 3 (EmptyTree)
printTree t
putStrLn("")
let t2 = insertIntoTree t 1
printTree t2
putStrLn("")
let t3 = removeFromTree t2 1
printTree t3
putStrLn("Size of the tree ")
print (treeSize t3)
putStrLn("Height of the tree ")
print (treeHeight t3)

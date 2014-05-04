data Tree c = EmptyTree | Node (Tree c) Char (Tree c)

formTreeString :: Tree c -> String
formTreeString (Node treeLeft c treeRight) = ['n'] ++ [c] ++ formTreeString(treeLeft) ++ formTreeString(treeRight)
formTreeString (EmptyTree) = ['e']

stringToTree = fst . buildPair
    where
              buildPair ('e':xs) = (EmptyTree, xs)
              buildPair ('n':value:values) = (Node leftSubtree value rightSubtree, rightRest) 
                                                            where
                                                                 (rightSubtree, rightRest) = buildPair leftRest
                                                                 (leftSubtree, leftRest) = buildPair values
              buildPair x = (EmptyTree, x)

printTree :: Tree c -> IO()
printTree EmptyTree = return()
printTree (Node leftSubtree value rightSubtree) = do
                                                                         putStr [value]
                                                                         printTree leftSubtree
                                                                         printTree rightSubtree
                                                                
main = do
let tree = Node (Node EmptyTree 'a' EmptyTree)  'b'  (Node EmptyTree 'c' EmptyTree) 
putStrLn("Tree : ")
printTree(tree)
putStrLn(" ")
putStrLn("Tree string : ")
let s = formTreeString(tree)
putStrLn(s)
putStrLn("Tree from this string : ")
printTree(stringToTree $ s)
putStrLn(" ")
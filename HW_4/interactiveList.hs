addToSortedList :: [Int] -> Int -> [Int]
addToSortedList [] value = [value]
addToSortedList (x:xs) value = if (x < value) then (x:(addToSortedList xs value)) else (value:x:xs)

removeFromSortedList :: [Int] -> Int -> [Int]
removeFromSortedList [] value = []
removeFromSortedList (x:xs) value = rmHelper [] (x:xs) value where
                                                    rmHelper scanned [] _ = scanned
                                                    rmHelper scanned (x:xs) v | (x /= v) = rmHelper (x:scanned) xs v
                                                                                         | otherwise  = (scanned ++ xs)
loop :: [Int] -> IO ()
loop list = do
    putStrLn("### What do you want to do now?")
    putStrLn("### Commands : exit,add,rm,print")
    command <- getLine
    case command of 
                             "exit" -> return ()
                             "add" -> do 
			      putStr("# What value? : ")
			      value <- readLn :: IO Int
			      loop (addToSortedList list value)
                             "rm" -> do 
			      putStr("# What value? : ")
			      value <- readLn :: IO Int
			      loop (removeFromSortedList list value)
                             "print" -> do 
                                        putStr("# Your sorted list is : ")
                                        print list
                                        loop list
                             _ -> do 
                                        putStrLn("Uninterpreted query...")
                                        loop list
main = loop []
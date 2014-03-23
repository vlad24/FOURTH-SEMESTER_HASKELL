conditionList :: [a] -> (a->Bool) -> Bool
conditionList (x:xs) condition | condition x = conditionList xs condition
                                       | otherwise = False

main = print( conditionList [2,4,5] even )
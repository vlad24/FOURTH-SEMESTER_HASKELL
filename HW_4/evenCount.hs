evenCount1 :: [Int] -> Int
evenCount1 list = length (filter even list)

evenCount2 :: [Int] -> Int
evenCount2 list = sum $ map ( \element -> (element + 1) `mod` 2) list

evenCount3 :: [Int] -> Int
evenCount3 list = foldr (\element accumulator -> accumulator + ((element + 1) `mod` 2)) 0 list
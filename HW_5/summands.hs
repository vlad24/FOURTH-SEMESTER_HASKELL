-- Algorythm is : write x and then concat it with all reprensentations of n - x 
sLists :: Int -> [[Int]]
sLists 1 = [[1]]
sLists n = [n] : concat [  map (\list -> x : list) (sLists (n - x)) | x <- [1..(n-1)]  ]

main = print(sLists 3)

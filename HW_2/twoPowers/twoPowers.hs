--Inserting into head of a list - optimization, better than inserting into a tail
--div 2^n 2  - optimization, considering that div k 2 is a bit shift
generateTwoPowers :: Int -> [Int]
generateTwoPowers n = powerChopper [] (2^n)
    where
    powerChopper :: [Int] -> Int -> [Int]
    powerChopper list 0 = list
    powerChopper list twoPower = powerChopper (twoPower:list) (div twoPower 2)
--
main :: IO ()
main = do
    putStr("Enter the max degree of your future list of powers of two : ")
    n <- readLn
    putStr("Your list: ")
    print(generateTwoPowers(n))
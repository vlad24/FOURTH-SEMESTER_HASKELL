bigBrother :: [Int] -> IO ()
bigBrother [] = print("No Big brother")
bigBrother (z:[]) = print("No Big brother")
bigBrother (y:z:[]) = print ("No Big brother")
bigBrother (x:y:z:zs) = if ((y > x) && (y > z)) then
                                    print("Big brother : " ++ show y)
                                else
                                    bigBrother (y:z:zs)
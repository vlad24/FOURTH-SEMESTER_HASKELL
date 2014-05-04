bigBrother :: [Int] -> IO ()
bigBrother [] = print("No Big brother")
bigBrother (z:[]) = print("No Big brother")
bigBrother (y:z:[]) = print ("No Big brother")
bigBrother (x:y:z:zs) = return y >>= \v -> if ((x < v) && (z < v)) then
                                                              print(show y)
                                                            else bigBrother (y:z:zs)
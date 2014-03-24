magicSum :: (Eq a, Floating a) => [a] -> a
magicSum (x:xs) = magicSumer (x:xs) 0 1 where
                                                            magicSumer [] sum product | (product /= 0) = (sum / product)
                                                                                                   | otherwise = error "some cos was a zero"
                                                            magicSumer (x:xs) sum product = magicSumer xs (sum + x) (product * cos(x))
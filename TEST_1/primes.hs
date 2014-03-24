primes = 2 : filtByHead[2*q + 1 | q <- [1..]] where filtByHead (x : xs) = x : filtByHead [ r | r <- xs, r `mod` x /= 0]

main = print (take 10 primes)
perm 0 = [[]]
perm n = concatMap (\list -> [(1 : list), (2 : list), (3 : list)]) (perm(n-1))
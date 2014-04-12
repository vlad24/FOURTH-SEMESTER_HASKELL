combs 1 = [1]
combs n = [1..n] >>= \s ->
                map (*s) [1..n]
-- func factor list = map (\y -> y * factor) list
-- get rid of the first argument
-- func factor = map (\y -> y * factor)
-- get rid of lambda expression
-- func factor = map (* factor)
-- get rid of the second argument
    func = map.(*)
-- So a 'points-free' definition of a function is one which does not explicitly mention the points (values) of the space on which the function acts.
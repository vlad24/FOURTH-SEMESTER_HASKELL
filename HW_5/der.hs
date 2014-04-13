data Polynom a =  Variable |
                          Constant a | 
                          Power (Polynom a) Int |
                          Addition (Polynom a) (Polynom a) |
                          Subtraction (Polynom a) (Polynom a) |
                          Multiplication (Polynom a) (Polynom a) |
                          Division (Polynom a) (Polynom a)
der (Variable) = (Constant 1)
der (Constant a) = (Constant 0)
der (Power Variable 1) = (Constant 1)
der (Power Variable n) = Multiplication (Constant n) (Power (Variable) (n - 1))
der (Power p n) = Multiplication ( Constant (n - 1) ) ( Multiplication (Power p (n - 1)) (der p) )
der (Addition p q) = Addition (der p) (der q)
der (Subtraction p q) = Subtraction (der p) (der q)
der (Multiplication p q) = Addition  ( Multiplication (der p) q ) ( Multiplication p (der q) ) 
der (Division p q) = Division (    Subtraction (Multiplication (der p) q) (Multiplication p (der q))      ) (Power q 2)


ease (Addition p (Constant 0)) = p
ease (Addition (Constant 0) p) = p
ease (Subtraction p (Constant 0)) = p
ease (Subtraction (Constant 0) p) = Multiplication (Constant (-1)) p
ease (Multiplication (Constant 0) p) = Constant 0
ease (Multiplication p (Constant 0)) = Constant 0
ease (Multiplication (Constant 1) p) = p
ease (Multiplication p (Constant 1)) = p
ease (Multiplication (Constant a) (Constant b)) = Constant (a * b)
ease (Division p (Constant 1)) = p
ease (Division (Constant 0) p) = (Constant 0)
ease (Power (Constant 0) _) = (Constant 0)
ease (Power _ 0) = (Constant 1)
ease (Power p 1) = p
ease (Power (Constant 1) _) = (Constant 1)
ease p = p

easePolynom (Addition p q) = ease (Addition (easePolynom p) (easePolynom q))
easePolynom (Subtraction p q) = ease (Subtraction (easePolynom p) (easePolynom q))
easePolynom (Multiplication p q) = ease (Multiplication (easePolynom p) (easePolynom q))
easePolynom (Division p q) = ease (Division (easePolynom p) (easePolynom q))
easePolynom (Power p n) = ease (Power (easePolynom p) n)
easePolynom p = p

instance Show a => Show (Polynom a) where
             show (Constant a) = show a
             show (Variable) = "x"
             show (Addition p q) = "(" ++ show p ++ "+" ++ show q ++ ")"
             show (Subtraction p q) = "(" ++ show p ++ "-" ++ show q ++ ")"
             show (Multiplication p q) = "(" ++ show p ++ "*" ++ show q ++ ")"
             show (Division p q) = "(" ++ show p ++ "/" ++ show q ++ ")"
             show (Power p n) = "(" ++ show p ++ ")^" ++ show n

differentiate = easePolynom.der

main = do
   let polynom = Power (Addition (Multiplication (Constant 0) (Power Variable 2)) (Addition (Multiplication (Constant 2) Variable) (Constant 1))) (24)
   putStr ("d((0x^2 + 2x + 1)^24)/dx = ")
   putStrLn (show $ differentiate polynom)


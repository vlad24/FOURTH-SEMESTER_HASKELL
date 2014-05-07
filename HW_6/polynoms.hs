import Data.Function (on)
import Data.List (sortBy)

--MonomList - coef then power

data Polynom a =  Monoms[ (Int, Int) ]

instance Show (Polynom a) where
                      show (Monoms []) = show()
                      show (Monoms(m:[])) = show (fst m) ++ show 'x' ++ show '^' ++ show(snd m)
                      show (Monoms (m:ms)) = show (fst m) ++ show 'x' ++ show '^' ++ show(snd m) ++ show('+') ++ show(Monoms(ms))

brushPolynom (Monoms []) = (Monoms [])
brushPolynom (Monoms (m:ms)) = brushHelper (Monoms (ms)) m []
                                                   where
                                                   brushHelper (Monoms []) powerAccum list = (Monoms (powerAccum : list))
                                                   brushHelper (Monoms (m:ms)) powerAccum list | ((snd m) == (snd powerAccum))  =  brushHelper (Monoms ms) (fst powerAccum + fst m, snd m)                   list
                                                                                                                     | otherwise                                 =  brushHelper (Monoms ms)                m                                    (powerAccum : list)

normalizePolynom (Monoms []) = (Monoms [])
normalizePolynom (Monoms monoms) = Monoms(sortBy (compare `on` snd) (monoms))

makePretty (Monoms monoms) = brushPolynom(normalizePolynom $ Monoms monoms)

sumPolynoms (Monoms []) (Monoms []) = (Monoms [])
sumPolynoms (Monoms (m:ms)) (Monoms []) = (Monoms (m:ms))
sumPolynoms (Monoms []) (Monoms (n:ns)) = (Monoms (n:ns))
sumPolynoms (Monoms (m:ms)) (Monoms (n:ns)) = sumPretty ( makePretty $ (Monoms (m:ms)) ) (makePretty $ (Monoms (n:ns)) ) (Monoms [])
                                                                              where
                                                                              sumPretty ( Monoms [] ) ( Monoms [] ) (Monoms(acc)) = normalizePolynom $ (Monoms acc)
                                                                              sumPretty ( Monoms [] ) ( Monoms (n:ns) ) (Monoms(acc)) = normalizePolynom $ (Monoms((n:ns)++acc))
                                                                              sumPretty ( Monoms (m:ms) ) ( Monoms [] ) (Monoms(acc)) = normalizePolynom $ (Monoms((m:ms)++acc))
                                                                              sumPretty ( Monoms (m:ms) ) ( Monoms (n:ns) ) (Monoms(acc)) | ((snd m) == (snd n)) = sumPretty ( Monoms (ms) ) ( Monoms (ns) ) (  Monoms( (fst m + fst n, snd m):acc )  )
                                                                                                                                                                       | ((snd m) > (snd n)) = sumPretty ( Monoms (ms) ) ( Monoms (n:ns) ) (  Monoms( (fst m, snd m):acc ) )
                                                                                                                                                                       | otherwise = sumPretty ( Monoms (m:ms) ) ( Monoms (ns) ) (  Monoms( (fst n, snd n):acc ) )
multMonomPolynom (c,p) (Monoms []) = (Monoms [])
multMonomPolynom (c,p) (Monoms (m:ms)) = makePretty(multMonPolHelper (c,p) (Monoms(m:ms)) (Monoms [] )) where
                                                                    multMonPolHelper (c,p) (Monoms([])) (Monoms acc) = (Monoms acc)
                                                                    multMonPolHelper (c,p) (Monoms(m:ms)) (Monoms (acc)) = multMonPolHelper (c,p) (Monoms ms) (Monoms( (c * fst m, p + snd m) : acc ))

multPolynoms (Monoms []) (Monoms monoms) = (Monoms [])
multPolynoms (Monoms monoms) (Monoms []) = (Monoms [])
multPolynoms (Monoms (m:ms)) (Monoms monoms) = multPolynomsHelper (Monoms (m:ms)) (Monoms monoms) (Monoms []) where
                                                                            multPolynomsHelper (Monoms []) (Monoms monoms) (Monoms acc) = (Monoms acc)
                                                                            multPolynomsHelper (Monoms (m:ms)) (Monoms monoms) (Monoms acc) =  multPolynomsHelper (Monoms ms) (Monoms monoms) ( sumPolynoms (Monoms acc) (multMonomPolynom m (Monoms monoms)) )

main = do
	print $ show(multPolynoms (Monoms([(1,0),(1,1)])) (Monoms([(2,0),(1,1)])))
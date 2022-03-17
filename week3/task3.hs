main :: IO()
main = do
    print $ calcSeriesSum 1 0 == -2.0 
    print $ calcSeriesSum 1 1 == -0.6666666666666667
    print $ calcSeriesSum 1 2 == -1.2000000000000002
    print $ calcSeriesSum 1 3 == -1.047619047619048
    print $ calcSeriesSum 1 4 == -1.0814814814814817
    print $ calcSeriesSum 1 5 == -1.0753246753246755
    print $ calcSeriesSum 1 6 == -1.0762718762718764

calcSeriesSum :: Double -> Double -> Double
calcSeriesSum x n = helper 0 0 (-2) 1 1
   where
       helper :: Double -> Double -> Double -> Double -> Double -> Double 
       helper result i nom denomMult denom
        | i > n = result
        | otherwise = helper (result + nom*x**i / denom) (i + 1) (nom*(-2)) (denomMult + 2) (denom * (denomMult + 2))
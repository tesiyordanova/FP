main :: IO()
main = do 
    print $ (averageFunction [(+1), (**0.5),(2**)]) 2

averageFunction :: (Fractional a, Num a) => [(a -> a)] -> (a -> a)
averageFunction fs = (\x -> sum [f x | f <- fs ] / (fromIntegral $ length fs))
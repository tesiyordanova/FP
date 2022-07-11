main :: IO()
main = do 

    print $ (poly [0 .. 3]) 1

poly :: [Int] -> (Int -> Int)
poly as = (\x -> sum [m * (x ^ n) | (m, n) <- zip as [0 .. ]])
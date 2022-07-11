main :: IO()
main = do 

    print $ factorize 13
    print $ factorize 152
    print $ factorize 123

factorize :: Int -> [Int]
factorize n = helper n 2 []
    where
     helper :: Int -> Int -> [Int] -> [Int]
     helper n i result
      | n <= 1 || i > n = result
      | isPrime i && mod n i == 0 = helper (div n i) i (result ++ [i])
      | otherwise = helper n (i + 1) result 

isPrime :: Int -> Bool
isPrime n =  n > 1 && all (\x -> mod n x /= 0)  [2 .. n - 1]
main :: IO()
main = do 
    print $ sumDivisibleNumbers 0 10 5 == 5
    print $ sumDivisibleNumbers 0 100 5 == 990
    print $ sumDivisibleNumbers 100 0 5 == 990

sumDivisibleNumbers :: Int -> Int -> Int -> Int
sumDivisibleNumbers start finish k = helper (min start finish) 
   where
       helper :: Int -> Int 
       helper i 
         | i > max start finish = 0
         | mod (sumDigits i) k == 0 = i + helper (i + 1) 
         | otherwise = helper (i + 1) 

sumDigits :: Int -> Int
sumDigits 0 = 0
sumDigits n = mod n 10 + sumDigits (div n 10)
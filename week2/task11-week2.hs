main :: IO()
main = do
     print $ truncatablePrime 3797 == True 
     print $ truncatablePrime 47 == False 

truncatablePrime :: Int -> Bool 
truncatablePrime x
   | x <= 9 && isPrime x = True
   | isPrime x = truncatablePrime (div x 10)
   | otherwise = False

isPrime :: Int -> Bool
isPrime n = n > 1 && helper 2
 where
     helper :: Int -> Bool
     helper i
      | (fromIntegral i) > (sqrt $ fromIntegral n) = True
      | mod n i == 0 = False
      | otherwise = helper (i + 1)
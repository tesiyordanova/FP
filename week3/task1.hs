main :: IO()
main = do
    print $ removeFistOccurrence 15365  5 == 1536
    print $ removeFistOccurrence 15360 0 == 1536
    print $ removeFistOccurrence 15300 0 == 1530
    print $ removeFistOccurrence 15365 1 == 5365
    print $ removeFistOccurrence 35365 3 == 3565
    print $ removeFistOccurrence 1212 1 == 122
    print $ removeFistOccurrence 1212 2 == 121

rev :: Int -> Int
rev n = helper n 0
 where
     helper :: Int -> Int -> Int
     helper 0 result = result
     helper n result = helper (div n 10) (result * 10 + mod n 10)

removeFistOccurrence :: Int -> Int -> Int 
removeFistOccurrence n digit = helper n 0
 where 
     helper :: Int -> Int -> Int           
     helper 0 result = result
     helper n result
      | mod n 10 == digit && result == 0 = div n 10   -- 12345 3 
      | mod n 10 == digit = rev result  + 10 ^ (countDigits result) * (div n 10)
      | otherwise = helper (div n 10) (result * 10 + mod n 10)

countDigits :: Int -> Int
countDigits x
   | div x 10 == 0 = 1
   | otherwise =  1 + countDigits (div x 10)
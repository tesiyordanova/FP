main :: IO()
main = do 
    print $ sortN 1714 -- == 7411
    print $ sortN 123405 -- == 543210
    print $ sortN 123045 == 543210
    print $ sortN 120345 == 543210
    print $ sortN 102345 == 543210
    print $ sortN 8910 == 9810
    print $ sortN 321 == 321
    print $ sortN 29210 == 92210
    print $ sortN 1230 == 3210
    print $ sortN 55345 == 55543
    print $ sortN 14752 == 75421
    print $ sortN 329450 == 954320
    print $ sortN 9125 == 9521

findZeros :: Int -> Int
findZeros x 
   | x < 10 = if x == 0 then 1 else 0
   | mod x 10 == 0 = 1 + findZeros(div x 10)
   | otherwise = findZeros (div x 10)

sortN :: Int -> Int
sortN x = helper x 0    
   where 
       helper :: Int -> Int -> Int 
       helper 0 result = result -- * 10 ^ (findZeros x)
       helper leftover result = helper (removeFistOccurrence leftover (findMax leftover))  (result * 10 + findMax leftover)
     

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
      | mod n 10 == digit && result == 0 = div n 10
      | mod n 10 == digit = rev result  + 10 ^ (countDigits result) * (div n 10)
      | otherwise = helper (div n 10) (result * 10 + mod n 10)

countDigits :: Int -> Int
countDigits x
   | div x 10 == 0 = 1
   | otherwise =  1 + countDigits (div x 10)

findMax :: Int -> Int
findMax n = helper n 0
    where 
        helper :: Int -> Int -> Int
        helper 0 currMax = currMax
        helper num currMax
         | (mod num 10) > currMax = helper (div num 10) (mod num 10)
         | otherwise = helper (div num 10) currMax



main :: IO()
main = do
    print $ removeD 2 2123 == 13
    print $ removeD 1 656 == 656
    print $ removeD 5 656 == 66
    print $ removeD 6 656 == 5
    print $ removeD 0 606 == 66
    print $ removeD 0 600 == 6
    print $ removeD 6 600 == 0

removeD :: Int -> Int -> Int 
removeD digit x = rev (remove digit x)

remove :: Int -> Int -> Int
remove digit x
   | x < 0 = error "x was negative"
   | otherwise = helper x 0
    where 
       helper :: Int -> Int -> Int
       helper 0 result = result
       helper x result
         | mod x 10 /= digit = helper (div x 10) (result * 10 + mod x 10)
         | otherwise = helper (div x 10) result

rev :: Int -> Int
rev n = helper n 0
 where
     helper :: Int -> Int -> Int
     helper 0 result = result
     helper n result = helper (div n 10) (result * 10 + mod n 10)

    
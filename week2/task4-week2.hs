main :: IO()
main = do 
    print $ countOccurences 121 1 == 2

countOccurences :: Int -> Int -> Int
countOccurences x digit
   | x == digit = 1
   | mod x 10 == digit = 1 + countOccurences (div x 10) digit
   | otherwise = countOccurences (div x 10) digit
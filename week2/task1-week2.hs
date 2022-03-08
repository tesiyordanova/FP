main :: IO()
main = do
    print $ countDigitsIter 12345 == 5
    print $ countDigitsIter 123 == 3

    print $ countDigitsRec 12345 == 5
    print $ countDigitsRec 123 == 3

countDigitsRec :: Int -> Int
countDigitsRec x
   | x < 0 = error "x was negative"
   | div x 10 == 0 = 1
   | otherwise =  1 + countDigitsIter (div x 10)


countDigitsIter :: Int -> Int 
countDigitIter x
   | x < 0 = error "x was negative"
   | otherwise = helper x 0
    where 
       helper :: Int -> Int -> Int
       helper 0 result = result 
       helper x result = helper (div x 10) (result + 1)
       

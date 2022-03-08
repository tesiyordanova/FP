main :: IO()
main = do
    print $ sumDigitsIter 12345 == 15
    print $ sumDigitsIter 123 == 6
    print $ sumDigitsIter 4 == 4

sumDigitsIter :: Int -> Int
sumDigitsIter x
   | x < 0 = error "x was negative"
   | otherwise = helper x 0
    where 
        helper :: Int -> Int -> Int 
        helper 0 result = result 
        helper x result = helper (div x 10) (result + mod x 10)
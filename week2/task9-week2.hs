main :: IO()
main = do
    print $ everyOther 12 == 1
    print $ everyOther 852369 == 628
    print $ everyOther 1714 == 11
    print $ everyOther 12345 == 42
    print $ everyOther 891 == 9
    print $ everyOther 123 == 2
    print $ everyOther 2121 == 22
    print $ everyOther 4736778 == 767
    print $ everyOther 448575 == 784
    print $ everyOther 4214 == 14

everyOther :: Int -> Int
everyOther x
   | x <= 9 = error "x has to be >= 10"
   | otherwise = helper x 0  
    where 
        helper :: Int -> Int -> Int 
        helper x result
         | x <= 9 = result
         | x <= 99 = div x 10 + result * 10
         | otherwise =  helper (div x 100) (result * 10 + mod (div x 10) 10)


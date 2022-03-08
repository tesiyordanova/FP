main :: IO()
main = do
    print $ areAmicable 200 300 == False
    print $ areAmicable 220 284 == True
    print $ areAmicable 284 220 == True
    print $ areAmicable 1184 1210 == True
    print $ areAmicable 2620 2924 == True
    print $ areAmicable 6232 6368 == True

areAmicable :: Int -> Int -> Bool
areAmicable x y = sumDivs x == sumDivs y

sumDivs :: Int -> Int
sumDivs x
   | x < 0 = error "wrong"
   | otherwise = helper 1 0
    where 
        helper :: Int -> Int -> Int
        helper currentDiv result
         | currentDiv > x = result
         | mod x currentDiv == 0 = helper (currentDiv + 1) (result + currentDiv)
         | otherwise = helper (currentDiv + 1) result

      
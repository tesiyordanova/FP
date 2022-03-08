main :: IO()
main = do

    print $ isInteresting 410 == True
    print $ isInteresting 212 == False
    print $ isInteresting 567 == False
    print $ isInteresting 70 == True 
    print $ isInteresting 5 == True 
    print $ isInteresting 4 == True 

sumDigitsRec :: Int -> Int
sumDigitsRec 0 = 0
sumDigitsRec n = mod n 10 + sumDigitsRec (div n 10)

isInteresting :: Int -> Bool
isInteresting x = mod x (sumDigitsRec x) == 0



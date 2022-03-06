main :: IO()
main = do 
    print $ myGcdG 5 13 == 1
    print $ myGcdG 13 1235 == 13

    print $ myGcdPM 5 13 == 1
    print $ myGcdPM 13 1235 == 13

myGcdG :: Int -> Int -> Int
myGcdG x y
   | x == 0 = y
   | y == 0 = x
   | x == y = x
   | x > y = myGcdG (x - y) y
   | y > x = myGcdG x (y - x)

myGcdPM :: Int -> Int -> Int
myGcdPM x 0 = x
myGcdPM 0 y = y
myGcdPM x y = myGcdPM y ( mod x y)




import Data.List
main :: IO()
main = do 

   print $ myPoly [2.7, 3.0 ..] 2.2 3 == -0.4399999999999998

myPoly :: [Double] -> Double -> Int -> Double
myPoly ls x y = product $ myList ls x y
    where
    myList _ _ 0 = []
    myList (l:ls) x y = [x - l] ++ myList ls x (y - 1)
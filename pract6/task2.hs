import Data.Char
import Data.List
main :: IO()
main = do 

    print $ digits 4321 == [4, 3, 2, 1]
    print $ decreasing [4, 3, 3, 1] == False
    print $ decDigits 4321
    print $ decDigits 4444
    print $ decDigits 4322

decDigits :: Int -> Bool
decDigits = decreasing . digits 

digits :: Int -> [Int]
digits = map digitToInt . show

decreasing :: [Int] -> Bool
decreasing xs = sort xs == reverse xs && (length $ nub xs) == length xs
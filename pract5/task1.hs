main :: IO()
main = do 

    print $ removeEveryKth 3 [1 .. 9]
    print $ removeEveryKth 4 [1 .. 7]

removeEveryKth :: Int -> [a] -> [a] 
removeEveryKth k xs = [m | (m, n) <- zip xs [1 ..], mod n k /= 0]
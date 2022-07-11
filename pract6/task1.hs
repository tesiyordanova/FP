main :: IO()
main = do 

    print $ listOfIndexes 3 [1, 2, 3, 4, 3, 5, 3, 2, 1]

listOfIndexes :: Int -> [Int] -> [Int]
listOfIndexes n xs = [k | (m, k) <- zip xs [0 ..], m == n]
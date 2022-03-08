main :: IO()
main = do
    print $ findSum 0 2 10 == 3578 -- 510 + 1022 + 2046
    print $ findSum 5 3 5 == 174 -- 26 + 50 + 98

findSum :: Int -> Int -> Int -> Int
findSum a b n = helper a 0
 where 
     helper :: Int -> Int -> Int 
     helper currentNum pow
      | pow >= n = 0
      | n - 3 <= pow && pow <= n - 1 = currentNum + 2^pow * b + helper (currentNum + 2^pow * b) (pow + 1)
      | otherwise = helper (currentNum + 2^pow * b) (pow + 1)
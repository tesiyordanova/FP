main :: IO()
main = do
    print $ countPalindromes 5 13 == 5 -- 6 7 8 9 11
    print $ countPalindromes 13 5 == 5 -- 6 7 8 9 11

rev :: Int -> Int
rev n = helper n 0
 where
     helper :: Int -> Int -> Int
     helper 0 result = result
     helper n result = helper (div n 10) (result * 10 + mod n 10)

isPalindrome :: Int -> Bool
isPalindrome x = rev x == x

countPalindromes :: Int -> Int -> Int
countPalindromes start finish = helper (min start finish) 0
 where
     helper :: Int -> Int -> Int
     helper i result
      | i == (max start finish) = result
      | isPalindrome (i+1)  = helper (i+1) (result + 1)
      | otherwise = helper (i+1) result
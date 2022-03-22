import Data.Char

main :: IO()
main = do
    print $ sumSpecialPrimes 5 2 == 392 -- n = 5, d = 2
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462

sumSpecialPrimes n d = sum $ take n $ filter (\x -> isPrime x && elem  (intToDigit d) (show x) ) [1 ..]

isPrime :: Int -> Bool
isPrime n = n > 1 && [1, n] == [ d | d <- [1 .. n], mod n d == 0]   


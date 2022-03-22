main :: IO()
main = do
    print $ sumUnevenLC 5 50 == 621
    print $ sumUnevenLC 50 1 == 625
    print $ sumUnevenLC 564 565 == 565

    print $ sumUnevenHOF 5 50 == 621
    print $ sumUnevenHOF 50 1 == 625
    print $ sumUnevenHOF 564 565 == 565

sumUnevenLC :: Int -> Int -> Int
sumUnevenLC start finish = sum [ d | d <- [min start finish .. max start finish], odd d]

sumUnevenHOF :: Int -> Int -> Int
sumUnevenHOF start finish = sum $ filter (\x -> odd x) [min start finish .. max start finish]
main :: IO()
main = do
    print $ p 1 == 1
    print $ p 3 == 12 -- +7
    print $ p 2 == 5 -- +4
    print $ p 4 == 22 -- + 10
    print $ p 5 == 35 -- + 13
    print $ p 6 == 51 -- + 16

p :: Int -> Int 
p 1 = 1
p n = 1 + 3 * (n - 1) + p (n - 1)

main :: IO()
main = do 
    
    print $ closestToAvarage [(Temp 1 23.6), (Temp 6 24.2), (Temp 11 24.2), (Temp 16 21.2), (Temp 21 23.8), (Temp 26 26.5), (Temp 31 24.5)] 


avg :: [Measuring] -> Float
avg temps = sum (listTemps temps) / (fromIntegral $ length (listTemps temps))
    
listTemps :: [Measuring] -> [Float]
listTemps temps = [t | (Temp day t) <- temps]

--closestToAvarage :: [Measuring] -> Int
closestToAvarage temps = fst $ foldr1 (\ (a, b) (c, d) -> if (abs $ avg temps - b ) < (abs $ avg temps - d) then (a, b) else (c, d)) vector
     where
         vector = [(d, t) | (Temp d t) <- temps]

data Measuring = Temp Int Float
    deriving (Show, Eq)

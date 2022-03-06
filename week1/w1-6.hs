main :: IO()
main = do
    print $ canCarry 5 15 3 == True
    print $ canCarry 1 5 4 == True
    print $ canCarry 13 25 2 == False
    print $ canCarry 24 104.44 21.12 == False
    print $ canCarry 51 34.75 19.852 == False
    print $ canCarry 42 95.11 0.51 == True

    print $ canCarry (-13) 25 2 
    print $ canCarry 13 (-25) 2  
    print $ canCarry 13 25 (-2)



canCarry :: Int -> Double -> Double -> Bool
canCarry c k w
  | c < 0 = error "error: The number of products was negative"
  | k < 0 = error "error: John's hosting capacity was negative"
  | w < 0 = error "error: The weight of a product was negative"
  | fromIntegral c * w <= k = True
  | otherwise = False

import Data.List
main :: IO()
main = do 

    print $ getClosestDistance [(ThreeD 4 5 6), (ThreeD 2 5 10)] == (4.47213595499958,ThreeD 4.0 5.0 6.0,ThreeD 2.0 5.0 10.0)
    print $ getClosestDistance [(ThreeD 4 5 6), (ThreeD 2 5 10), (ThreeD 5 2 (-10)), (ThreeD (-2) 1 45), (ThreeD 12 0 2)] == (4.47213595499958,ThreeD 4.0 5.0 6.0,ThreeD 2.0 5.0 10.0)
    print $ getClosestDistance [(ThreeD 4 5 6), (ThreeD 2 5 10), (ThreeD 5 2 (-10)), (ThreeD (-2) 1 45), (ThreeD 12 0 2), (ThreeD 6 5 4)] == (2.8284271247461903,ThreeD 4.0 5.0 6.0,ThreeD 6.0 5.0 4.0)

    print $ getClosestDistance [(TwoD 4 6), (TwoD 5 10), (TwoD 5 29), (TwoD 1 45), (TwoD 0 2), (TwoD 69 42)] == (4.123105625617661,TwoD 4.0 6.0,TwoD 5.0 10.0)

data Point a = TwoD a a | ThreeD a a a
    deriving (Show, Eq)

getClosestDistance :: (Num a, Floating a, Ord a) => [Point a] -> (a, Point a, Point a)
getClosestDistance ps = foldl1 (\v1@(d1, _, _) v2@(d2, _, _) -> if d1 < d2 then v1 else v2) $  map (\[p1, p2] -> (findDistance p1 p2, p1, p2)) $ filter (\x -> length x == 2) (subsequences ps) 


findDistance :: (Num a, Floating a) => Point a -> Point a -> a
findDistance (TwoD x1 y1) (TwoD x2 y2) = sqrt $ (x1 - x2)**2 + (y1 - y2)**2
findDistance (ThreeD x1 y1 z1) (ThreeD x2 y2 z2) = sqrt $ (x1 - x2)**2 + (y1 - y2)**2 + (z1 - z2)**2
findDistance _ _ = error "Different dimentions" 
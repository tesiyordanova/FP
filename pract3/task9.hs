main :: IO()
main = do 

    print $ flatten (List []) == []
    print $ flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) == [1,2,3,4,5]
    print $ flatten (Elem 1) == [1]

data NestedList = Elem Int | List [NestedList]
 deriving (Show, Eq)

--flatten :: NestedList -> [Int]
flatten (List []) = []
flatten (Elem x) = [x]
flatten (List xs) = concat $ map flatten xs
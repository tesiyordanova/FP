main :: IO()
main = do 

    print $ deepestLeavesSum t3

deepestLeavesSum :: BTree -> Int 
deepestLeavesSum t = sum $ head $ reverse $ takeWhile (/= []) $ map (getLevel t) [0 .. ]

getLevel :: BTree -> Int -> [Int]
getLevel Empty _ = []
getLevel (Node v _ _) 0 = [v]
getLevel (Node v left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)

t3 :: BTree
t3 = (Node 1 (Node 2 (Node 4 (Node 7 Empty Empty) Empty) (Node 5 Empty Empty)) (Node 3 Empty (Node 6 Empty (Node 8 Empty Empty))))

data BTree = Empty | Node Int BTree BTree
    deriving (Show, Eq)


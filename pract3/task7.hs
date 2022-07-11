main :: IO()
main = do 

    print $ grandchildrenIncreased t1 == False
    print $ grandchildrenIncreased t2 == True

t1 = Node 8 (Node 3 (Node 1 Empty Empty) (Node 4 Empty Empty)) (Node 10 (Node 9 Empty Empty) (Node 14 Empty Empty))

t2 = Node 8 (Node 3 (Node 9 Empty Empty) (Node 10 Empty Empty)) (Node 10 (Node 11 Empty Empty) (Node 14 Empty Empty))

data BTree = Empty | Node Int BTree BTree

--grandchildrenIncreased :: BTree -> Bool
grandchildrenIncreased Empty = True
grandchildrenIncreased t@(Node x left right) = all (\l -> l > x) (getLevel t 2) && grandchildrenIncreased left && grandchildrenIncreased right
-- grandchildrenIncreased (Node x left right) = all (\l -> l > x) (getLevel left 1) &&  all (\r -> r > x) (getLevel right 1) && grandchildrenIncreased left && grandchildrenIncreased right

--getLevel :: TBree -> Int -> [Int]
getLevel Empty _ = []
getLevel (Node x left right) 0 = [x]
getLevel (Node _ left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)
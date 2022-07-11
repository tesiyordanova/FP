main :: IO()
main = do 

    print $ isSymmetric Empty == True
    print $ isSymmetric t3 == False
    print $ isSymmetric t4 == True
    print $ isSymmetric t5 

data BTree = Empty | Node Int BTree BTree
    deriving (Show, Eq)

isSymmetric :: BTree -> Bool
isSymmetric Empty = True
isSymmetric (Node _ Empty Empty) = True
isSymmetric (Node x left right) = (getLevel left 0 == getLevel right 0) && (isSymmetric left == isSymmetric right)

getLevel Empty _ = []
getLevel (Node x _ _) 0 = [x]
getLevel (Node _ left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)

t3 :: BTree                         --   1
t3 = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)    -- 2   3

t4 :: BTree                                 --     1
t4 = Node 1 (Node 2 (Node 3 Empty Empty) Empty) (Node 2 Empty (Node 3 Empty Empty))   -- 3       3

t5 :: BTree                                         --       1
t5 = Node 1 (Node 2 (Node 3 Empty Empty) (Node 7 Empty (Node 5 Empty Empty))) (Node 2 (Node 7 Empty (Node 5 Empty Empty)) (Node 3 Empty Empty))        
main :: IO()
main = do 

    print $ isBalanced t1
    print $ isBalanced t2

isBalanced ::  BTree -> Bool
isBalanced Empty = True 
isBalanced (Node v left right) = (abs $ height left - height right) <= 1 && isBalanced left && isBalanced right
    -- where
height tree = length $ takeWhile (/= []) $ map (getLevel tree) [0 .. ]

getLevel :: BTree  -> Int -> [Int]
getLevel Empty _ = []
getLevel (Node x left right) 0 = [x]
getLevel (Node _ left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)

data BTree = Empty | Node Int BTree BTree
    deriving (Show, Eq)

t1 = (Node 5 Empty (Node 4 (Node 5 Empty Empty) (Node 7 Empty Empty)))

t2 = (Node 5 (Node 3 Empty Empty) (Node 4 (Node 5 Empty Empty) (Node 7 Empty Empty)))
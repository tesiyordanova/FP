main :: IO()
main = do 
    print $ levelSum numberBTree 1 == 11 -- (5 + 6)
    print $ cone numberBTree == True

cone :: BTree -> Bool
cone Nil = True 
cone t = helper (takeWhile (/=[]) $ map (getLevel t) [0 ..])
 where
     helper [] = True
     helper [x] = True
     helper (x:xs) = sum x < sum (head xs) && helper xs

levelSum :: BTree -> Int -> Int
levelSum t k = sum $ getLevel t k 

getLevel :: BTree  -> Int -> [Int]
getLevel Nil _ = []
getLevel (Node x left right) 0 = [x]
getLevel (Node _ left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)

numberBTree :: BTree
numberBTree = (Node 10 (Node 5 (Node 1 Nil Nil) (Node 9 Nil Nil)) (Node 6 (Node 8 Nil Nil) (Node 7 Nil Nil)))

data BTree = Nil | Node Int BTree BTree
 deriving (Show, Eq)
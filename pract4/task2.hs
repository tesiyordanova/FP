main :: IO()
main = do 

    print $ convert tree == (Node 30 (Node 36 (Node 36 Nil Nil) (Node 35 Nil (Node 33 Nil Nil))) (Node 21 (Node 26 Nil Nil) (Node 15 Nil (Node 8 Nil Nil))))

convert :: BTree -> BTree 
convert Nil = Nil 
convert t@(Node value _ _) = helper t 
    where
        helper Nil = Nil
        helper (Node v left right) = (Node  (sum [x | x <- list t, x >= v]) (helper left) (helper right))

list :: BTree -> [Int]
list Nil = []
list (Node v left right) = [v] ++ list left ++ list right

data BTree = Nil | Node Int BTree BTree
 deriving (Show, Eq)

tree :: BTree 
tree = (Node 4 (Node 1 (Node 0 Nil Nil) (Node 2 Nil (Node 3 Nil Nil))) (Node 6 (Node 5 Nil Nil) (Node 7 Nil (Node 8 Nil Nil))))

import Data.List
main :: IO()
main = do 

    print $ leavesAreEqual t1 t2 == True
    print $ leavesAreEqual t3 t4 == False

leavesAreEqual :: BTree -> BTree -> Bool
leavesAreEqual tr1 tr2 = (reverse $ sort $ listLeaves tr1) == (reverse $ sort $ listLeaves tr2)
    where
    listLeaves Nil = []
    listLeaves (Node v Nil Nil) = [v]
    listLeaves (Node _ left right) = listLeaves left ++ listLeaves right


data BTree = Nil | Node Int BTree BTree
    deriving (Show, Eq)

t1 :: BTree
t1 = (Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 26 Nil Nil) (Node 32 Nil Nil))))

t2 :: BTree 
t2 = (Node 25 (Node 10 (Node 1 Nil Nil) Nil) (Node 30 (Node 32 Nil Nil) (Node 26 Nil Nil)))

t3 :: BTree
t3 = (Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 26 Nil Nil) (Node 32 Nil Nil))))

t4 :: BTree
t4 = (Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 27 Nil Nil) (Node 32 Nil Nil))))
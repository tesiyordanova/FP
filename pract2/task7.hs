main :: IO()
main = do


data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)

numberTreeBefore :: BTree Int
numberTreeBefore = Node 10 (Node 5 (Node 3 Nil Nil) (Node 7 Nil Nil)) (Node 15 Nil (Node 18 Nil Nil))

insert :: BTree a -> a -> BTree a 
insert Nil k = Node k Nil Nil
insert (Node x left right) k   
    | 
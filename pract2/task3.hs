import Data.List
main :: IO()
main = do 

    print $ ordered t1 == True
    print $ ordered t2 == False

data BTree a = Nil | Node (a, a) (BTree a) (BTree a)
    deriving (Show, Eq) 

t1 = Node (3, 10) (Node (5, 8) (Node (6, 7) Nil Nil) (Node (4, 9) Nil Nil) ) (Node (2, 12) Nil (Node (1, 15) Nil Nil))

t2 = Node (3, 10) (Node (5,8) (Node (6, 7) Nil Nil) (Node (7, 9) Nil Nil)) (Node (2, 12) Nil Nil)

ordered :: (Eq a, Ord a) => BTree a -> Bool
ordered tree = traverseDFSx tree == (reverse $ sort $ traverseDFSx tree) && traverseDFSy tree == (sort $ traverseDFSy tree)

traverseDFSy ::BTree a -> [a]
traverseDFSy Nil = []
traverseDFSy (Node (x, y) left right) = traverseDFSy left ++ [y] ++ traverseDFSy right


traverseDFSx :: BTree a -> [a]
traverseDFSx Nil = []
traverseDFSx (Node (x, y) left right) = traverseDFSx left ++ [x] ++ traverseDFSx right
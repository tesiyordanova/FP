main :: IO()
main = do 
    
    print $ t
    print $ size t
    print $ getLevel t 2

data Tree a = Nil | Node a [Tree a]
 deriving (Show, Eq)

t :: (Num a) => Tree a
t = Node 10 [Node 3 [Node 5 [Nil], Node 8 [Node 1 [Nil], Node 2 [Nil]], Node 9 [Nil]], Node 7 [Node 11 [Nil], Node 13 [Nil]], Node 12 [Node 6 [Nil], Node 4 [Nil]]] 

size :: Tree a -> Int
size Nil = 0 
size (Node _ children) = 1 + (sum $ map size children)

getLevel :: Tree a -> Int -> [a]
getLevel Nil _ = []
getLevel (Node x _) 0 = [x]
getLevel (Node _ children ) k = concat $ map (\ch -> getLevel ch (k - 1)) children
main :: IO()
main = do 

    print $ numOfNodes t == 2

data Tree a = Nil | Node a [Tree a]
 deriving (Show, Eq)

t :: (Num a) => Tree a
t = Node 10 [Node 3 [Node 5 [Nil], Node 8 [Node 1 [Nil], Node 2 [Nil]], Node 9 [Nil]], Node 7 [Node 11 [Nil], Node 13 [Nil]], Node 12 [Node 6 [Nil], Node 4 [Nil]]] 

-- numOfNodes :: Tree a -> Int
numOfNodes Nil = 0 
numOfNodes tree@(Node x children ) = length [sum chs | chs <- map (\c -> getLevel c 1 ) children, x == sum chs ] + (sum $ map numOfNodes children)
    -- where 
    --     k = 1 + head [ l | (ns, l) <- zip (takeWhile (/= []) $ map (getLevel tree) [0 .. ]) [0 ..], elem x ns]


getLevel :: Tree a -> Int -> [a]
getLevel Nil _ = []
getLevel (Node x _) 0 = [x]
getLevel (Node x children) k = concat $ map (\ch -> getLevel ch (k - 1)) children
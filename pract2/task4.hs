main :: IO()
main = do 

     print $ isGraceful t1 == True
     print $ isGraceful t2 == False

t1 = Node 1 [Node 3 [Nil], Node 5 [Nil], Node 7 [Nil], Node 9 [Nil]]

t2 = Node 7 [Node 9 [Node 5 [Nil], Node 2 [Nil]]]

data Tree a = Nil | Node a [Tree a]
    deriving (Show)

isGraceful :: (Eq a, Num a, Integral a) => Tree a -> Bool
isGraceful Nil = True 
isGraceful (Node _ [Nil]) = True
isGraceful tree@(Node x chs) = all (\l -> mod (x - l) 2 == 0 && all (\c -> isGraceful c) chs) (getLevel tree k)
    where 
        k = 1 + head [ l | (ns, l) <- zip (takeWhile (/= []) $ map (getLevel tree) [0 .. ]) [0 ..], elem x ns]

getLevel :: Tree a -> Int -> [a]
getLevel Nil _ = []
getLevel (Node x _ ) 0 = [x]
getLevel (Node _ chs) k = concat $ map (\x -> getLevel x (k - 1)) chs 
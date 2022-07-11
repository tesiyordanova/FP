import Data.List
main :: IO()
main = do

    print $ height numberBTree == 4
    print $ height charBTree  == 3

    print $ average numberBTree -- == 16.22
   --print $ average charBTree -- should not work

    print $ sumLeaves numberBTree == 119
    --print $ sumLeaves charBTree -- shouldn't work

    print $ areEqual numberBTree (Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 4 (Node 2 Nil Nil) (Node 5 Nil Nil))) == False
    print $ areEqual charBTree charBTree == True
    print $ areEqual numberBTree (Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 8 (Node 2 Nil Nil) (Node 5 Nil (Node 21 Nil Nil)))) == False

    print $ setLevels numberBTree == Node (0,5) (Node (1,12) (Node (2,1) (Node (3,96) Nil Nil) Nil) (Node (2,0) Nil Nil)) (Node (1,4) (Node (2,2) Nil Nil) (Node (2,5) Nil (Node (3,21) Nil Nil)))
    print $ setLevels charBTree == Node (0,'k') (Node (1,'a') (Node (2,'h') Nil Nil) (Node (2,'s') Nil Nil)) (Node (1,'l') (Node (2,'e') Nil Nil) (Node (2,'l') Nil Nil))

    print $ mirrorTree numberBTree == Node 5 (Node 4 (Node 5 (Node 21 Nil Nil) Nil) (Node 2 Nil Nil)) (Node 12 (Node 0 Nil Nil) (Node 1 Nil (Node 96 Nil Nil)))
    print $ mirrorTree charBTree == Node 'k' (Node 'l' (Node 'l' Nil Nil) (Node 'e' Nil Nil)) (Node 'a' (Node 's' Nil Nil) (Node 'h' Nil Nil))

mirrorTree :: BTree a -> BTree a 
mirrorTree Nil = Nil
mirrorTree (Node x left right) = Node x (mirrorTree right) (mirrorTree left)

areEqual :: (Eq a) => BTree a -> BTree a -> Bool
areEqual = (==)
-- areEqual t1 t2 = t1 == t2

setLevels :: BTree a -> BTree (Int, a)
setLevels tree = helper tree 0 -- Node (findLevel tree, x) (setLevels left) (setLevels right)  
   where 
       helper Nil _ = Nil
       helper (Node value left right) k = Node (k, value) (helper left (k + 1)) (helper right (k + 1))
    --  findLevel tree = head [ l | (leaves, l) <- levels tree, elem x leaves]

-- levels tree = zip (takeWhile (/= []) $ map (getLevel tree) [0 .. ]) [0 ..]

sumLeaves :: (Num a) => BTree a -> a
sumLeaves Nil = 0
sumLeaves (Node x Nil Nil) = x
sumLeaves (Node _ left right) = sumLeaves left + sumLeaves right

height :: (Eq a) => BTree a -> Int
height tree = length $ takeWhile (/= []) $ map (getLevel tree) [0 .. ]

getLevel :: BTree a -> Int -> [a]
getLevel Nil _ = []
getLevel (Node x left right) 0 = [x]
getLevel (Node _ left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)

round'  = (/100) . fromIntegral . round . (*100)

average :: (Num a, Integral a) => BTree a -> Double
average tree = round' $ fromIntegral (sum tree) / fromIntegral (size tree) 
    where 
    sum Nil = 0
    sum (Node x left right) = x + sum left + sum right

    size Nil = 0
    size (Node _ left right) = 1 + size left + size right

data BTree a = Nil | Node a (BTree a) (BTree a)
    deriving (Show, Eq) 

numberBTree :: BTree Int
numberBTree = Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 4 (Node 2 Nil Nil) (Node 5 Nil (Node 21 Nil Nil)))

charBTree :: BTree Char 
charBTree = Node 'k' (Node 'a' (Node 'h' Nil Nil) (Node 's' Nil Nil)) (Node 'l' (Node 'e' Nil Nil) (Node 'l' Nil Nil))
import Data.List
main :: IO()
main = do 

    print $ allContain [t1, t2] -- == ["acd","cd","d"]
    print $ allContain [t1, t2, t3] == []
    print $ allContain [t3, t4] == ["g"]

    
    print $ allContain' [t1, t2] -- == ["acd","cd","d"]
    print $ allContain' [t1, t2, t3] == []
    print $ allContain' [t3, t4] == ["g"]

-- allContain :: (Ord a) => [BTree a] -> [[a]]
allContain tss@(t:ts) = helper (nub $ allWords tss) tss 
 where
    allWords [] = []
    allWords (t:ts) = genWords t ++ allWords ts

    helper res [] = res
    helper res (t:ts) = helper (filter (\ ws -> elem ws (genWords t)) res) ts

allContain' = foldr1 intersect . map genWords


genWords :: (Eq a, Ord a) => BTree a -> [[a]]
genWords t = sort $ filter (\ x -> containsWord t x && not (null x)) $ subsequences $ listWords t
    where
        listWords Nil = []
        listWords (Node v left right) = [v] ++ listWords left ++ listWords right

containsWord :: (Eq a) => BTree a -> [a] -> Bool
containsWord _ [] = True
containsWord (Node v Nil Nil) [x] = v == x
containsWord (Node v left right) (x:xs)
    | v == x = helper left xs || helper right xs
    | otherwise = containsWord left (x:xs) || containsWord right (x:xs)
        where
            helper (Node v Nil Nil) [x] = v == x
            helper (Node v left right) (x:xs)
             | v == x = helper left xs || helper right xs
             | otherwise = False
            helper _ _ = False
containsWord _ _ = False

data BTree a = Nil | Node a (BTree a) (BTree a) 
    deriving (Show, Eq)

t1 :: BTree Char
t1 = Node 'a' (Node 'c' (Node 'f' Nil Nil) (Node 'd' Nil Nil)) (Node 'b' Nil (Node 'e' Nil Nil))

t2 :: BTree Char
t2 = Node 'a' (Node 'c' (Node 'd' Nil Nil) Nil) (Node 'b' Nil Nil)

t3 :: BTree Char
t3 = Node 'a' (Node 'b' (Node 'd' (Node 'h' Nil Nil) (Node 'i' Nil Nil)) (Node 'e' Nil Nil)) (Node 'c' (Node 'f' Nil Nil) (Node 'g' Nil Nil)) 

t4 :: BTree Char
t4 = Node 'a' (Node 'c' (Node 'd' Nil Nil) Nil) (Node 'g' Nil Nil)
main :: IO()
main = do 
    print $ rangedSum firstTree 100 50 == 0 -- (L = 100, R = 50)
    print $ rangedSum firstTree 7 15 == 32 -- (L = 7, R = 15)
    print $ rangedSum firstTree 15 7  == 32 -- (L = 15, R = 7)
    print $ rangedSum secondTree 6 10 == 23 -- (L = 6, R = 10)
    print $ rangedSum secondTree 10 6 == 23 -- (L = 10, R = 6)


rangedSum :: Tree -> Int-> Int -> Int
rangedSum Nil _ _ = 0
rangedSum (Node v left right) x y = if v >= min x y && v <= max x y then v + rangedSum left x y + rangedSum right x y else rangedSum left x y + rangedSum right x y

firstTree :: Tree
firstTree = Node 10 (Node 5 (Node 3 Nil Nil) (Node 7 Nil Nil)) (Node 15 Nil (Node 18 Nil Nil))

secondTree ::Tree 
secondTree = Node 10 (Node 5 (Node 3 (Node 1 Nil Nil) Nil) (Node 7 (Node 6 Nil Nil) Nil)) (Node 15 (Node 13 Nil Nil) (Node 18 Nil Nil))

data Tree = Nil | Node Int Tree Tree
 deriving (Show, Eq)


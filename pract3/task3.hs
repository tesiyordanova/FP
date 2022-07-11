main :: IO()
main = do 

    print $ maxDepthBlueNode colorTree == 2

colorTree = Node Blue (Node Red (Node Green Empty Empty) Empty) (Node Red (Node Blue (Node Green Empty Empty) (Node Red Empty Empty)) Empty)

maxDepthBlueNode :: Tree -> Int
maxDepthBlueNode t = maximum $ [ l | (colors, l) <- zip (takeWhile (/= []) $ map (getLevel t) [0 .. ]) [0 ..], elem Blue colors]

getLevel :: Tree -> Int -> [Color]
getLevel Empty _ = []
getLevel (Node x left right) 0 = [x]
getLevel (Node _ left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)


data Color = Red | Green | Blue
    deriving (Show, Eq)

data Tree = Empty | Node Color Tree Tree
    deriving (Show, Eq) 
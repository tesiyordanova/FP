import Data.List
main :: IO()
main = do 

    print $ rf (Song "Mozart""The Marriage of Figaro Overture" 270) == "Summertime"
    print $ rf (Song "Gershwin""Summertime" 300) == "Rhapsody in Blue"
    print $ rf (Song "Gershwin""Rhapsody in Blue" 1100) == "Rhapsody in Blue"

    print $ toBST t1 == (Node 2 (Node 1 Empty Empty) (Node 7 (Node 5 Empty Empty) (Node 8 Empty Empty)))
    print $ toBST t2 == (Node 1 (Node 0 Empty Empty) Empty)
    print $ toBST t3 == (Node 5 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 8 (Node 7 Empty Empty) (Node 10 Empty Empty)))

--task1
recommender :: Playlist -> (Song -> SongName)
recommender (Playlist pl) = (\(Song author name duration) -> head $ filter (\x -> x /= []) $ [ n | (Song auth n dur) <- pl, auth == author && dur > duration] ++ [ n | (Song auth n dur) <- pl, dur > duration] ++ [name])

rf :: Song -> SongName
rf = recommender (Playlist songs)

songs = [(Song "Mozart" "The Marriage of Figaro Overture" 270), (Song "Gershwin" "Summertime" 300), (Song "Queen" "Bohemian Rhapsody" 355), (Song "Gershwin" "Rhapsody in Blue" 1100)]

type AuthorName = String
type SongName = String
type SongLength = Int
data Song = Song AuthorName SongName SongLength
 deriving (Eq)
data Playlist = Playlist [Song]

--task2

toBST :: (Ord a) => BTree a -> BTree a
toBST Empty = Empty 
toBST t = helper t (head $ filter (\(x, y) -> x /= y) $ zip (traverseDFS t) (sort $ traverseDFS t))
    where 
     helper Empty _ = Empty
     helper (Node value left right) v@(m, n)
      | value == n = Node m (helper left v) (helper right v)
      | value == m = Node n (helper left v) (helper right v)
      | otherwise = Node value (helper left v) (helper right v)

traverseDFS :: BTree a -> [a]
traverseDFS Empty = []
traverseDFS (Node x left right) = traverseDFS left ++ [x] ++ traverseDFS right

data BTree a = Empty | Node a (BTree a) (BTree a)
    deriving (Eq, Show)

t1 = Node 2 (Node 5 Empty Empty) (Node 7 (Node 1 Empty Empty) (Node 8 Empty Empty))

t2 = Node 0 (Node 1 Empty Empty) Empty

t3 = Node 5 (Node 2 (Node 1 Empty Empty) (Node 8 Empty Empty)) (Node 3 (Node 7 Empty Empty) (Node 10 Empty Empty))



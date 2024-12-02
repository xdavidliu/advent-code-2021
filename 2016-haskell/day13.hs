import Data.Bits (popCount, (.&.))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

main = do
    putStr "part 1 = "
    print solveOne
    putStr "part 2 = "
    print solveTwo

input = 1358
target = (31,39)
start = (1,1)

type Pos = (Int, Int)
isOpen :: Pos -> Bool
isOpen (x,y) = 0 == 1 .&. popCount z
  where z = input + x * (x + 3 + 2*y) + y * (1 + y)

nearby :: Pos -> [Pos]
nearby (0,0) = [(0,1),(1,0)]
nearby (0,y) = [(0,y+1),(0,y-1),(1,y)]
nearby (x,0) = [(x-1,0),(x+1,0),(x,1)]
nearby (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

valid :: Set Pos -> Pos -> Bool
valid seen p = isOpen p && not (p `Set.member` seen)

solveOne = fst $ bfs (maxBound :: Int)
                 (Seq.singleton (start,0)) (Set.singleton start)
solveTwo = Set.size $ snd $ bfs 50 (Seq.singleton (start,0))
                            (Set.singleton start)

bfs :: Int -> Seq (Pos, Int) -> Set Pos -> (Int, Set Pos)
bfs lim q seen = case Seq.viewl q of
    Seq.EmptyL -> (-1, seen)
    (p, d) Seq.:< qRest -> explore lim (d+1) nb qRest seen where
        nb = filter (valid seen) (nearby p)

explore :: Int -> Int -> [Pos] -> Seq (Pos, Int) -> Set Pos -> (Int, Set Pos)
explore lim _ [] q seen = bfs lim q seen
explore lim d (p:ps) q seen
  | d > lim = explore lim d ps q seen
  | p == target = (d, seen)
  | otherwise = explore lim d ps (q Seq.|> (p,d)) (p `Set.insert` seen)

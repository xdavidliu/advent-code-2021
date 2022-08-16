import Distribution.Utils.MD5 (md5)
import Data.ByteString.Char8 (pack)
import Data.Char (digitToInt)

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq


input = "pgflpeqp"
main = do
  putStr "part 1 = "
  putStrLn $ solvePart1 ""
  putStr "part 2 = "
  print $ solvePart2 ""

md5digest z = show (md5 $ pack z)
open c = 10 < digitToInt c 
strIf _ False = ""
strIf s True = s

position :: String -> (Int, Int)
position s = rec s (0,0)
  where rec "" t = t
        rec ('U':xs) (a,b) = rec xs (a, b-1)
        rec ('D':xs) (a,b) = rec xs (a, b+1)
        rec ('L':xs) (a,b) = rec xs (a-1, b)
        rec ('R':xs) (a,b) = rec xs (a+1, b)
        rec _ _ = error "rec"

solvePart1 s = bfs (Seq.singleton s) s True
solvePart2 s = length $ bfs (Seq.singleton s) s False

bfs :: Seq String -> String -> Bool -> String
bfs q t short = case Seq.viewl q of
  Seq.EmptyL -> t
  s Seq.:< qRest -> explore s qRest short t

goal = (3,3)

explore s q short t = enqueue udlr s q short tt
  where (u:d:l:r:_) = md5digest $ input ++ s
        p@(x,y) = position s
        isGoal = p == goal
        tt = if isGoal then s else t
        uu = strIf "U" (y /= 0 && open u)
        dd = strIf "D" (y /= 3 && open d)
        ll = strIf "L" (x /= 0 && open l)
        rr = strIf "R" (x /= 3 && open r)
        udlr = if isGoal then "" else dd ++ rr ++ uu ++ ll

enqueue "" s q short t = bfs q t short
enqueue (x:xs) s q short t
  | short && position k == goal = k
  | otherwise = enqueue xs s (q Seq.|> k) short t
  where k = s ++ [x]

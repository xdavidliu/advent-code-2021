import Distribution.Utils.MD5 (md5)
import Data.ByteString.Char8 (pack)
import Data.Char (digitToInt)

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq


--input = "pgflpeqp"
input = "ihgpwlah"
main = do
  putStr "part 1 = "
  putStrLn $ solvePart1 ""
  putStr "part 2 = "
  -- print $ solvePart2 ""

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

solvePart1 s = bfs (Seq.singleton s) s
solvePart2 s = length $ bfs (Seq.singleton s) s

bfs :: Seq String -> String -> String
bfs q t = case Seq.viewl q of
  Seq.EmptyL -> t
  s Seq.:< qRest -> explore s qRest

goal = (3,3)

explore s q = enqueue udlr s q
  where (u:d:l:r:_) = md5digest $ input ++ s
        (x,y) = position s
        uu = strIf "U" (y /= 0 && open u)
        dd = strIf "D" (y /= 3 && open d)
        ll = strIf "L" (x /= 0 && open l)
        rr = strIf "R" (x /= 3 && open r)
        udlr = dd ++ rr ++ uu ++ ll

enqueue "" s q = bfs q s
enqueue (x:xs) s q
  | position t == goal = t
  | otherwise = enqueue xs s (q Seq.|> t)
  where t = s ++ [x]

-- TODO: gotta STOP when hit (3,3), so don't use target

import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sort)

main = do
  han <- openFile "input.txt" ReadMode
  conts <- hGetContents han
  let lns = drop 2 $ lines conts
      ss = map sizes lns  -- second one is negated because of hack!
      as = sort $ map (abs . snd) ss
  putStr "part 1 = "
  print $ solveOne ss as
  putStr "part 2 = "
  print $ 2 + 22 + 7 + 5 * 30  -- see notes below
  hClose han

solveOne ss as = sum $ map fit ss
  where n = length ss
        set = Set.fromList $ zip as [n,n-1..]
        fit (0,_) = 0
        fit s@(u,na) = case Set.lookupGE s set of
          Nothing -> 0
          Just (_, k) -> if u <= abs na then k-1 else k

sizes :: String -> (Int, Int)
sizes ln = (read $ init u, negate $ read $ init a)
  where [u, a, _] = drop 2 $ words ln
-- hack, negate the a part so that we can compare it directly later on with
-- stuff from the set, AND know which a the u had, AND not have lexicography
-- mess things up

{-
if you look at my data, for x from 31 down to 25, at y=6 there's a 400-500 row
so consider that non-accessible space, like a wall. Then x=24 y=6 is normal.

Ah, and if I ctrl-f my input for " 0%" I see that _ is initially at (26,22),
so first, I need to move the _ around the wall to the upper right. Since x=24
is safe, need to move left twice from 26, then, up 22 times to y=0, then finally
from 24 to 31, which is 7 moves. Now the G is at x=30 and _ is at x=31, both
y=0. And we have 31 moves so far.

Now need to repeatedly move the G left one by one by having the _ go around.
Each move of G left requires _ to move 5 (draw it to see!) So to get the G from
x=30 to x=0, need 150 moves, so that's 181 total.
-}

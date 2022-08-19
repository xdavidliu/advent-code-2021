import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Control.Monad (forM_)
import Data.Array (listArray, assocs, (!))
import Data.Char (isDigit)
import Data.Sequence (Seq, viewl, (|>), ViewL(EmptyL, (:<)))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

main = do
    han <- openFile "test.txt" ReadMode
    conts <- hGetContents han
    let gr = grid (lines conts)
        digs = [(i,e) | (i,e) <- assocs gr, isDigit e]
    forM_ digs print
    print $ bfTreeDigs '0' digs gr
    hClose han

grid lns = listArray ((0,0), (nr-1,nc-1)) $ concat lns
  where nr = length lns
        nc = length (head lns)

bfTreeDigs e digs gr =
    bfs gr (Seq.singleton (i,0)) (Set.singleton i) [(e,0)] (length digs)
      where (i,_) = head [(i,c) | (i,c) <- digs, c == e]

bfs gr qu seen digSeen nDig = case viewl qu of
    EmptyL -> undefined  -- let explore return
    ((i, j), d)  :< quRest -> explore nbs d gr quRest seen digSeen nDig
      where nbs = neighbors (i,j) gr seen

-- assumes outer boundary is all # so they are sentinels and don't need to check
-- for out of bounds
neighbors (i,j) gr seen = filter good ps
  where ps = [(i+1,j),(i-1,j),(i,j+1),(i,j-1)]
        good t = '#' /= gr ! t && t `Set.notMember` seen

-- don't need to worry about passing through digits on the way to another digit,
-- since when we list the perms, that will be taken into account
explore [] _ gr qu seen digSeen nDig
  | nDig == length digSeen = digSeen
  | otherwise = bfs gr qu seen digSeen nDig
explore ((i,j):nbs) d gr qu seen digSeen nDig
  | nDig == length digSeen = digSeen
  | otherwise = explore nbs d gr quNext seenNext digSeenNext nDig
  where c = gr ! (i,j)
        quNext = qu |> ((i,j),d+1)
        seenNext = (i,j) `Set.insert` seen
        digSeenNext = if isDigit c then (c,d+1):digSeen else digSeen

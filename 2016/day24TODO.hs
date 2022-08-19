import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Data.Array (listArray, assocs, array, (!))
import Data.Char (isDigit, intToDigit)
import Data.Sequence (Seq, viewl, (|>), ViewL(EmptyL, (:<)))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

main = do
    han <- openFile "test.txt" ReadMode
    conts <- hGetContents han
    let gr = grid (lines conts)
        digs = [(i,e) | (i,e) <- assocs gr, isDigit e]
        tb = table digs gr
    print tb
    hClose han

grid lns = listArray ((0,0), (nr-1,nc-1)) $ concat lns
  where nr = length lns
        nc = length (head lns)

table digs gr = concat [els c (b i c) | (i,c) <- digs]
  where iQu i = Seq.singleton (i,0)
        iSeen i = Set.singleton i
        nDig = length digs
        b i c = bfs gr (iQu i) (iSeen i) [(c,0)] nDig
        foo x (y, d) = ((x, y), d)
        els c digSeen = map (foo c) digSeen
        hi = intToDigit $ nDig - 1
        bnds = (('0', '0'), (hi, hi))

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

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

main = do
  putStrLn "part 1 will take about 5 seconds"
  putStr "part 1 = "
  print $ solve inputOne
  putStrLn "part 1 will take about 10 MINUTES!!!!!"
  putStr "part 2 = "
  print $ solve inputTwo

type State = (Int, [Int])

{-
The first floor contains a thulium generator, a thulium-compatible microchip, a
  plutonium generator, and a strontium generator.
The second floor contains a plutonium-compatible microchip and a strontium-
  compatible microchip.
The third floor contains a promethium generator, a promethium-compatible
  microchip, a ruthenium generator, and a ruthenium-compatible microchip.
The fourth floor contains nothing relevant.
-}
inputOne :: State
inputOne = (0, [0,0,0,2,2,0,1,1,2,2])
-- input = (0, [1,2,0,0])  -- example from problem
-- first five are generators, last five are microchips
-- ordering are thulium, plutonium, strontium, promethium, ruthenium

inputTwo :: State
inputTwo = (0, [0,0,0,2,2,0,0,0,1,1,2,2,0,0])
-- just put two zeros at end of each size-5 half

ones :: Int -> [Int] -> [Int]
ones fl fls = [ i | (i, x) <- zip [0..] fls, x == fl]

twos :: Int -> [Int] -> [(Int, Int)]
twos fl fls = [ (i, j) | i <- os, j <- os, i < j] where os = ones fl fls

shiftOne :: Int -> Int -> [Int] -> [Int]
shiftOne i d fls = l ++ [d + head r] ++ tail r
  where (l, r) = splitAt i fls

shiftTwo :: (Int, Int) -> Int -> [Int] -> [Int]
shiftTwo (i,j) d fls = shiftOne i d (shiftOne j d fls)

neighbors :: State -> [State]
neighbors (fl, fls) = above ++ below where
  above | fl == 4-1 = [] | otherwise = map (stateD 1) (onesAndTwos 1)
  below | fl == 0 = [] | otherwise = map (stateD (-1)) (onesAndTwos (-1))
  onesAndTwos d = onesD d ++ twosD d
  stateD d fls = (fl + d, fls)
  onesD d = [shiftOne i d fls | i <- ones fl fls]
  twosD d = [shiftTwo (i,j) d fls | (i,j) <- twos fl fls]

valid :: State -> Bool
valid (fl, fls) = noGens || null loneChips where
  n = length fls `div` 2
  noGens = fl `notElem` take n fls
  loneChips = [i | i <- os, i >= n, (i-n) `notElem` os]
  os = ones fl fls

validFromPrev :: Int -> State -> Bool
validFromPrev prevFl st@(fl, fls) = valid (prevFl, fls) && valid st

validNeighbors :: State -> [State]
validNeighbors st@(fl, fls) = filter (validFromPrev fl) (neighbors st)

solve input = bfs (Seq.singleton (input, 0)) (Set.singleton input)

bfs :: Seq (State, Int) -> Set State -> Int
bfs que seen = case Seq.viewl que of
  Seq.EmptyL -> error "bfs"
  (st@(fl, fls), dist) Seq.:< rest -> explore nbs (1+dist) rest seen where
    nbs = filter notSeen $ validNeighbors st
    notSeen st = st `Set.notMember` seen
  
explore :: [State] -> Int -> Seq (State, Int) -> Set State -> Int
explore [] _ que seen = bfs que seen
explore (nb:nbs) dist que seen
  | nb == (3,replicate (2*n) 3) = dist
  | otherwise = explore nbs dist (que Seq.|> (nb, dist)) (nb `Set.insert` seen)
  where n = length (snd nb) `div` 2

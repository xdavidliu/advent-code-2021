import Data.List (elemIndex, sort)
import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )

main = do
  han <- openFile "input.txt" ReadMode
  conts <- hGetContents han
  let ps = sort $ map parse (lines conts)
  putStr "part 1 = "
  print $ partOne ps
  putStr "part 2 = "
  print $ partTwo ps
  hClose han

partOne ps@((0,_):_) = rec ps where
  rec ((a,b):(c,d):ps)
    | b + 1 < c = b + 1
    | otherwise = rec ((c, max b d):ps)
  rec _ = undefined
partOne _ = 0  -- if smallest interval not begin with 0, answer is 0

partTwo ps@((a,_):_) = rec ps a where
  rec ((a,b):(c,d):ps) acc
    = rec ((c, max b d):ps) (acc + max 0 (c-b-1))
  rec [(a,b)] acc = acc + max 0 (4294967295-b-1)
  rec _ _ = undefined
partTwo _ = undefined

-- Int is 64 bit on my machine, but not guaranteed to be big enough
parse :: String -> (Integer, Integer)
parse s = case elemIndex '-' s of
  Just i -> (read a, read b) where (a, _:b) = splitAt i s
  Nothing -> undefined

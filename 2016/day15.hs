import Control.Monad (forM_)
import System.IO (hClose, openFile, hGetContents, IOMode(ReadMode))

main = do
  han <- openFile "input.txt" ReadMode
  conts <- hGetContents han
  let abcs = map parse (lines conts)
      r@(a,b,c) = foldl1 bigger abcs
      x0 = (-a - c) `mod` b
      v x (a,b,c) = 0 == (x + a + c) `mod` b
      rest = [ t | t <- abcs, t /= r ]
      xs rest = [ i | i <- [x0,x0+b..], all (v i) rest ]
      lastDisc = (1 + length abcs, 11, 0)
      restPart2 = rest ++ [lastDisc]
  putStr "part 1 = "
  print $ head (xs rest)
  putStr "part 2 = "
  print $ head (xs restPart2)
  hClose han

parse :: String -> (Int, Int, Int)
parse ln = (a, b, c) where
  ws = words ln
  a = read $ tail (ws !! 1)
  b = read $ ws !! 3
  c = read (init $ last ws)

bigger u@(_,b,_) v@(_,y,_)
  | b > y = u
  | otherwise = v

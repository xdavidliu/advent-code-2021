import System.IO ( hClose, openFile, hGetLine, IOMode(ReadMode) )

main = do
  han <- openFile "input.txt" ReadMode
  ln <-  hGetLine han
  putStr "part 1 = "
  print $ solvePart1 ln
  putStr "part 2 = "
  print $ solvePart2 ln
  hClose han

countSafe row = sum [1 | x <- row, x == '.']
solve n input = sum $ map countSafe (take n $ iterate down input)
solvePart1 = solve 40
solvePart2 = solve 400000

trap "^^." = '^'
trap ".^^" = '^'
trap "..^" = '^'
trap "^.." = '^'
trap _ = '.'

rec "" (y:z:ur) "" = rec [y] (z:ur) [trap ['.',y,z]]
rec (x:ul) (y:z:ur) dl = rec (y:x:ul) (z:ur) (trap [x,y,z] : dl)
rec (x:_) [y] dl = reverse (trap [x,y,'.'] : dl)
rec _ _ _ = error "rec"

down row = rec [] row []

import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Perform ( parse, value, perform, Val(A))

main = do
    han <- openFile "input.txt" ReadMode
    conts <- hGetContents han
    let inss = map parse $ lines conts
        ans1 = perform [] inss (0,0,0,0)
        ans2 = perform [] inss (0,0,1,0)
    putStr "part 1 = "
    print $ value A ans1
    putStr "part 2 = "
    print $ value A ans2
    hClose han

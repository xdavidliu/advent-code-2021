import Data.List (sort, group)
import System.IO ( hClose, hGetContents, openFile, IOMode(ReadMode) ) 

extremal f xs = snd $ head xss where
    t ys = (f $ length ys, head ys)
    xss = sort (map t $ group $ sort xs)

mostCommon = extremal negate
leastCommon :: [Char] -> Char
leastCommon = extremal id

main = do
    han <- openFile "input.txt" ReadMode
    conts <- hGetContents han
    putStr "part 1 = "
    putStrLn $ map mostCommon (cols $ lines conts)
    putStr "part 2 = "
    putStrLn $ map leastCommon (cols $ lines conts)
    hClose han where
        cols lns = [map (!! i) lns | i <- [0..length (head lns) - 1]]

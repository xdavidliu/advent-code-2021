import Data.List (sort, elemIndices)
import System.IO (openFile, hGetContents, hClose, IOMode(ReadMode))
import Data.Char (ord, chr)
import Control.Monad (forM_)
import Data.Text (isInfixOf, pack)

count :: (Foldable t, Num b, Eq a) => a -> t a -> b
count c = foldl inc 0 where
    inc i x | c == x = i+1 | otherwise = i

popular cs = take 5 $ map snd ts where
    ts = sort [f c | c <- ['a'..'z']]
    f c = (negate $ count c cs, c)

sectorId cs
 | check == "[" ++ popular name ++ "]" = (map (rotate idNum) name, idNum)
 | otherwise = ("", 0) where
    idNum = read $ tail id
    (name, idAndCheck) = splitAt (last $ elemIndices '-' cs) cs
    (id, check) = splitAt (head $ elemIndices '[' idAndCheck) idAndCheck

rotate _ '-' = ' '
rotate n c = chr $ oa  + (ord c - oa + n) `mod` 26 where
    oa = ord 'a'

main = do
    han <- openFile "input.txt" ReadMode
    conts <- hGetContents han
    let ss = map sectorId $ lines conts
    putStrLn "part 1: "
    print $ sum (map snd ss)
    putStrLn "part 2: "
    let hasNorth s = pack "north" `isInfixOf` pack s 
    forM_ [s | s <- ss, hasNorth $ fst s] print
    hClose han
    

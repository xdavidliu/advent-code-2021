import System.IO (openFile, IOMode(ReadMode), hGetContents)
import Data.Char (ord, chr)

grab mv = foldl iter where
    iter cs ln = foldl mv (head cs) ln : cs

main = do
    han <- openFile "input.txt" ReadMode
    conts <- hGetContents han
    let lns = lines conts
    putStrLn $ "part 1 = " ++ tail (reverse $ grab mv1 "5" lns)
    putStrLn $ "part 2 = " ++ tail (reverse $ grab mv2 "7" lns)

incr c i = chr $ ord c + i

mv1 c 'D' | c `elem` "789" = c | otherwise = incr c 3
mv1 c 'U' | c `elem` "123" = c | otherwise = incr c (-3)
mv1 c 'L' | c `elem` "147" = c | otherwise = incr c (-1)
mv1 c 'R' | c `elem` "369" = c | otherwise = incr c 1
mv1 _ _ = error "mv1"

mv2 c 'L' | c `elem` "125AD" = c | otherwise = incr c (-1)
mv2 c 'R' | c `elem` "149CD" = c | otherwise = incr c 1
mv2 '1' 'D' = '3'
mv2 '2' 'D' = '6'; mv2 '3' 'D' = '7'; mv2 '4' 'D' = '8'
mv2 '6' 'D' = 'A'; mv2 '7' 'D' = 'B'; mv2 '8' 'D' = 'C'
mv2 'B' 'D' = 'D'
mv2 c 'D' | c `elem` "5ADC9" = c | otherwise = error "mv2 D"
mv2 '3' 'U' = '1'
mv2 '6' 'U' = '2'; mv2 '7' 'U' = '3'; mv2 '8' 'U' = '4'
mv2 'A' 'U' = '6'; mv2 'B' 'U' = '7'; mv2 'C' 'U' = '8'
mv2 'D' 'U' = 'B'
mv2 c 'U' | c `elem` "52149" = c | otherwise = error "mv U"
mv2 _ _ = error "mv2"

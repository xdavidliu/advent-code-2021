import System.IO
    ( hClose, openFile, hGetLine, IOMode(ReadMode) )
import Data.List (elemIndex)

times n xs = take k $ cycle xs where
    k = n * length xs

parse "" _ acc = acc
parse ('(':xs) f acc = parse trueRest f newAcc where
    (lhs,rhs,rest) = extract xs
    (toRep, trueRest) = splitAt lhs rest
    newAcc = acc + rhs * f toRep
parse (x:xs) f acc = parse xs f (1+acc)

parseOne s = parse s length 0
parseTwo s = parse s parseTwo 0

extract xs = (read lhs,read rhs,rest) where
    breakAt c xs = case elemIndex c xs of
        Just ix -> splitAt ix xs
        Nothing -> error "breakAt"
    (lhs,_:mid) = breakAt 'x' xs
    (rhs,_:rest) = breakAt ')' mid 

main = do
    han <- openFile "input.txt" ReadMode
    ln <- hGetLine han
    putStr "part 1 = "
    print (parseOne ln)
    putStr "part 2 = "
    print (parseTwo ln)
    hClose han

import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Control.Monad (forM_)
import Data.List (elemIndex)
import Data.Array ((//), listArray, (!), elems)

execute arr ins
  | head ws == "rect" = rect arr (pair second)
  | second == "row" = rotRow arr xy final
  | second == "column" = rotCol arr xy final
  | otherwise = error "execute"
  where ws = words ins
        second = ws !! 1
        xy = read $ drop 2 (ws !! 2)
        final = read $ last ws

pair cs = (read a, read $ tail b) where
    (a,b) = case elemIndex 'x' cs of
        Just i -> splitAt i cs
        Nothing -> error "pair"

rect arr (nc,nr) = arr // [((y,x), True) | y <- [0..nr-1], x <- [0..nc-1]]

rotRow arr y amt = arr // zip inds vals where
    inds = [(y, (x + amt) `mod` 50) | x <- [0..50-1]]
    vals = [ arr ! (y,x) | x <- [0..50-1]]

rotCol arr x amt = arr // zip inds vals where
    inds = [((y + amt) `mod` 6, x) | y <- [0..6-1]]
    vals = [ arr ! (y,x) | y <- [0..6-1]]

toChar b = if b then '#' else '.'

display arr = forM_ [0..6-1] (putStrLn . row) where
    row y = [ toChar (arr ! (y, x)) | x <- [0..50-1] ]

main = do
    han <- openFile "input.txt" ReadMode
    conts <- hGetContents han
    let lns = lines conts
        arr = listArray ((0,0), (6-1,50-1)) (repeat False)
        arrEnd = foldl execute arr lns
    putStr "part 1 = "
    print $ length (filter id $ elems arrEnd)
    putStrLn "part 2 = "
    display arrEnd
    hClose han

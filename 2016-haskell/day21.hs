import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Data.List (elemIndex)

startOne = "abcdefgh"
startTwo = "fbgdceah"

main = do
  han <- openFile "input.txt" ReadMode
  conts <- hGetContents han
  let inss = map parse $ lines conts
      ans1 = foldl perform startOne inss
      ans2 = foldl perform startTwo (map inverse $ reverse inss)
  putStr "part 1 = "
  print ans1
  putStr "part 2 = "
  print ans2
  hClose han

data Ins = SwapPos Int Int | SwapLetter Char Char | RotateRight Int
           | RotateLeft Int | RotatePosLetter Char | Reverse Int Int
           | Move Int Int | InverseRotatePosLetter Char deriving (Show)

inverse h@(SwapPos _ _) = h
inverse h@(SwapLetter _ _) = h
inverse (RotateRight x) = RotateLeft x
inverse (RotateLeft x) = RotateRight x
inverse (RotatePosLetter a) = InverseRotatePosLetter a
inverse h@(Reverse _ _) = h
inverse (Move x y) = Move y x
inverse _ = undefined

parse ln = case first2 of
  ["swap", "position"] -> SwapPos (read r) (read z)
  ["swap", "letter"] -> SwapLetter (head r) (head z)
  ["rotate", "right"] -> RotateRight (read r)
  ["rotate", "left"] -> RotateLeft (read r)
  ["rotate", "based"] -> RotatePosLetter (head z)
  ["reverse", _] -> Reverse (read r) (read z)
  ["move", _] -> Move (read r) (read z)
  _ -> undefined
  where (first2, r:rest) = splitAt 2 (words ln)
        z = last rest

perform :: String -> Ins -> String
perform s (SwapPos x y)
  | x > y = perform s (SwapPos y x)
  | x == y = s
  | otherwise = a ++ [q] ++ c ++ [p] ++ d
  where (a, p:b) = splitAt x s
        (c, q:d) = splitAt (y-x-1) b

perform s (SwapLetter a b) = case (elemIndex a s, elemIndex b s) of
  (Just x, Just y) -> perform s (SwapPos x y)
  _ -> undefined

perform s (RotateRight 0) = s
perform s (RotateRight k) = rotate s k True

perform s (RotateLeft 0) = s
perform s (RotateLeft k) = rotate s k False

perform s (RotatePosLetter c) = case elemIndex c s of
  Just i -> perform s (RotateRight k)
    where k = 1 + i + (if i >= 4 then 1 else 0)
  Nothing -> undefined

perform s (InverseRotatePosLetter c) = head p
  where ts = [ perform s (RotateRight i) | i <- [0..n-1]]
        n = length s
        p = [ t | t <- ts, s == perform t (RotatePosLetter c) ]

perform s (Reverse x y)
  | x == y = s
  | x > y = perform s (Reverse y x)
  | otherwise = a ++ reverse c ++ d
  where (a, b) = splitAt x s
        (c, d) = splitAt (y-x+1) b

perform s (Move x y)
  | x == y = s
  | otherwise = insertAt p y (a ++ b)
  where (a, p:b) = splitAt x s

insertAt p i s = a ++ [p] ++ b
  where (a, b) = splitAt i s

rotate s k z
  | g == 0 = s
  | otherwise = b ++ a
  where n = length s
        g = k `mod` n
        (a, b) = splitAt w s
        w = if z then n-g else g

import Data.Char (digitToInt, intToDigit)

main = do
    putStr "part 1 = "
    putStrLn $ solve 272
    putStr "part 2 = "
    putStrLn $ solve 35651584
    where solve gl = map intToDigit $ checksum gl $ pad gl input

input = map digitToInt "10010000000110000"

pad :: Int -> [Int] -> [Int]
pad gl a
  | gl <= length a = a
  | otherwise = pad gl $ a ++ [0] ++ b
  where b = map (1 -) (reverse a)

checksum :: Int -> [Int] -> [Int]
checksum gl a
  | l > gl = checksum gl (take gl a)
  | 1 == l `mod` 2 = a
  | otherwise = rec a []
  where l = length a
        rec [] acc = checksum gl $ reverse acc
        rec (x:y:zs) acc = rec zs (q:acc) where q = if x == y then 1 else 0
        rec _ _ = error "rec"

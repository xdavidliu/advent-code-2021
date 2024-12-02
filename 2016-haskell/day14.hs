import Data.Char (digitToInt)
import Distribution.Utils.MD5 (md5)
import Data.ByteString.Char8 (pack)

salt = "jlmsuwbz"

main = do
  putStr "part 1 = "
  print $ keysPart1 !! (64-1)
  putStrLn "part 2 will take about 5 minutes"
  putStr "part 2 = "
  print $ keysPart2 !! (64-1)
  where keysPart1 = keys 1
        keysPart2 = keys 2017

isKey :: Maybe Char -> [Int] -> Bool
isKey Nothing _ = False
isKey (Just c) window = window `hasChar` c

keys n = [ i | (i,trip,w) <- zip3 [0..] trips ws, isKey trip w] where
  zeros = replicate 16 0
  (first1000, rest1000) = splitAt 1000 fives
  w0 = foldl incCounts zeros first1000
  diff old new = decCounts (incCounts zeros new) old
  diffs = zipWith diff fives rest1000
  ws = zipWith (zipWith (+)) diffs (w0:ws)
  dig i = iterate md5digest (salt ++ show i) !! n
  digs = [ dig i | i <- [0..] ]
  trips = map triple digs
  fives = map collectFives digs

md5digest z = show (md5 $ pack z)

collectFives :: String -> String
collectFives cs = rec cs [] where
  rec "" xs = xs
  rec (a:t@(b:c:d:e:r)) xs
    | a `elem` xs = rec t xs
    | a == b && a == c && a == d && a == e = rec r (a:xs)
    | otherwise = rec t xs
  rec (_:t) xs = rec t xs

triple :: String -> Maybe Char
triple "" = Nothing
triple (a:t@(b:c:r))
  | a == b && b == c = Just a
  | otherwise = triple t
triple (_:t) = triple t

hasChar :: [Int] -> Char -> Bool
xs `hasChar` c = 0 < xs !! digitToInt c

updateCountSingle :: (Int -> Int -> Int) -> [Int] -> Char -> [Int]
updateCountSingle op xs c = bef ++ (k `op` 1) : aft where
  (bef, k : aft) = splitAt (digitToInt c) xs

updateCounts :: (Int -> Int -> Int) -> [Int] -> String -> [Int]
updateCounts op = foldl (updateCountSingle op)

incCounts = updateCounts (+)
decCounts = updateCounts (-)

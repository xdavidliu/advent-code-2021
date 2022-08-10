import Distribution.Utils.MD5 (md5)
import Data.ByteString.Char8 (pack)
import Data.Maybe (isJust, catMaybes)
import Data.List (sort)
import Data.Char (digitToInt)

md5digest z = show (md5 $ pack z)

passChar i
  | first5 == "00000" = Just $ take 2 rest
  | otherwise = Nothing
  where dig = md5digest $ "uqwqemis" ++ show i
        (first5, rest) = splitAt 5 dig

part2 ([ci,cv]:ps) qs
  | 8 == length qs = map (!! 1) $ sort qs
  | invalid = part2 ps qs
  | otherwise = part2 ps ([ci,cv]:qs) where
    rci = digitToInt ci
    invalid = rci < 0 || rci >= 8 || any (\ q -> ci == head q) qs
part2 _ _ = error "part 2"

main = do
    putStrLn "part 1 (this will take about 45 seconds):"
    putStrLn $ map head (take 8 found)
    putStrLn "part 2:"
    putStrLn $ part2 found []
    where
        found = catMaybes $ filter isJust [passChar i | i <- [0..]]

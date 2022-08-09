import System.IO (openFile, hClose, hGetContents, IOMode(ReadMode))

-- also in Data.List.Split, which requires separate package
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

everyNth n off xs = [a | (a, b) <- zip xs [0..], off == b `mod` n]

main = do
    han <- openFile "input.txt" ReadMode
    conts <- hGetContents han
    let ns = map read (words conts)
        nss1 = chunksOf 3 ns
        f i = everyNth 3 i ns
        nss2 = chunksOf 3 $ f 0 ++ f 1 ++ f 2
        ans nss = show (length $ filter triangle nss)
    putStrLn $ "part 1 = " ++ ans nss1
    putStrLn $ "part 2 = " ++ ans nss2
    hClose han

triangle ns = sum ns > 2 * maximum ns

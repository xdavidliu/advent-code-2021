import System.IO (openFile, IOMode(ReadMode), hGetContents, hClose)

dropLastIfComma cs
  | ',' == last cs = init cs
  | otherwise = cs

main = do
    han <- openFile "input.txt" ReadMode
    conts <- hGetContents han
    let dirs = concatMap (spread . dropLastIfComma) (words conts)
        (_,(x,y),_,rep) = foldl step ((0,1),(0,0),[],Nothing) dirs
    putStrLn $ "part 1 = " ++ show (dist x y)
    putStrLn $ "part 2 = " ++ part2 rep
    hClose han
    where
        dist x y = abs x + abs y
        part2 Nothing = "failed"
        part2 (Just (x,y)) = show $ dist x y
        spread r@(c:"1") = [r]
        spread (c:cs) = (c:"1") : replicate (-1 + read cs) "M1"
        spread _ = error "spread"

step (dir,pos,seen,rep) (c:cs) =
    (newDir, newPos, newPos : seen, newRep)
    where
        newRep = case rep of
            Just x -> Just x
            Nothing -> if newPos `elem` seen then Just newPos else Nothing
        newPos = straightLn pos change
        newDir = turn dir c
        change = scale (read cs) newDir
        straightLn (a,b) (c,d) = (a+c,b+d)
        scale c (a,b) = (a*c, b*c)
        turn (1,0) 'L' = (0,1)
        turn (1,0) 'R' = (0,-1)
        turn (-1,0) 'L' = (0,-1)
        turn (-1,0) 'R' = (0,1)
        turn (0,1) 'L' = (-1,0)
        turn (0,1) 'R' = (1,0)
        turn (0,-1) 'L' = (1,0)
        turn (0,-1) 'R' = (-1,0)
        turn pos 'M' = pos
        turn _ _ = error "turn"
step _ _ = error "step"

import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )

main = do
    han <- openFile "input.txt" ReadMode
    conts <- hGetContents han
    let lns = lines conts
        popi = map harvest lns
    putStr "part 1 = "
    print (length $ filter validOne popi)
    putStr "part 2 = "
    print (length $ filter validTwo popi)
    hClose han

validOne (po,pi) = outAbba && not inAbba where
    found p = not (all (null . getAbba) p)
    outAbba = found po
    inAbba = found pi

validTwo (po,pi) = revIntersect outBcb where
    outBcb = concatMap getBcb po
    inBcb = concatMap getBcb pi
    revIntersect [] = False
    revIntersect ((a,b):xs) = (b,a) `elem` inBcb || revIntersect xs

harvest xs = outside xs ("", []) ("", []) where
    outside "" (so, po) ("", pi) = (so:po, pi)
    outside ('[':xs) (so, po) ("", pi) = inside xs ("", so:po) ("", pi)
    outside (x:xs) (so, po) ("", pi) = outside xs (x:so, po) ("", pi)
    outside _ _ _ = error "outside"
    inside (']':xs) ("",po) (si,pi) = outside xs ("",po) ("",si:pi)
    inside (x:xs) ("",po) (si,pi) = inside xs ("",po) (x:si,pi)
    inside _ _ _ = error "inside"

getAbba xs = rec xs [] where
    rec "" ps = ps
    rec [_] ps = ps
    rec [_,_] ps = ps
    rec [_,_,_] ps = ps
    rec (a:b:c:d:xs) ps
      | a == d && a /= b && b == c = rec xs ((a,b):ps)
      | otherwise = rec (b:c:d:xs) ps

getBcb xs = rec xs [] where
    rec "" ps = ps
    rec [_] ps = ps
    rec [_,_] ps = ps
    rec (a:b:c:xs) ps
      | a == c && a /= b = rec (b:c:xs) ((a,b):ps)
      | otherwise = rec (b:c:xs) ps

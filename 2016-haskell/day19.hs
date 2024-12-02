input = 3005290

main = do
  putStr "part 1 = "
  print $ stepUntilOne [1..input]
  putStr "part 2 = "
  print $ solvePart2 input

step xs = rec xs []
  where rec (x:_:xs) acc = rec xs (x:acc)
        rec [x] acc = x : reverse acc
        rec [] acc = reverse acc

stepUntilOne [x] = x
stepUntilOne [] = undefined
stepUntilOne xs = stepUntilOne (step xs)

rec 0 1 = 1
rec 0 x = rec 1 (x-1)
rec r x
  | x <= d = x
  | x <= 2*d = 2*x-d
  | otherwise = rec (r+1) (x-2*d)
  where d = 3^(r-1)

solvePart2 = rec 0

foo (x:xs) = l ++ r ++ [x]
  where (l, _:r) = splitAt k xs
        k = -1 + (1 + length xs) `div` 2
foo [] = undefined

bar n = head $ iterate foo [1..n] !! (n-1)

{-
empirically compute and examine pattern and guess the closed form

*Main> take 60 $ map bar [1..]
1,
1,3,
1,2,3,5,7,9,
1,2,3,4,5,6,7,8,9,11,13,15,17,19,21,23,25,27,
1,2,3,4,5 ... 24,25,26,27,29,31,33,35,37 ... 77,79,81,
1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]

r = 0, 1, 2
1..3^(r-1) then by twos up to 3^r

row r has 2 * 3^(r-1) elements, except for r = 0 which has 1
first part is 3^(r-1), second part is (3^r - 3^(r-1)) / 2 = 3^(r-1)
-}

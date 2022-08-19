{- x 0 0 0         NOTE: scroll down below to see the haskell code
cpy a d

cpy 4 c

cpy 633 b
  x 633 4 x
inc d

dec b

jnz b -2              6,8,10,12 -> d += 633, b = 0
  x 0 4 x+633
dec c

jnz c -5            2,4,6,8,10,12,14,16 -> b = 0, c = 0, d = y
                            y = x + 4 * 633
cpy d a
  
jnz 0 0

cpy a b

cpy 0 a
  0 y 0 y         y = x + 4 * 633
cpy 2 c
  
jnz b 2        starting from y, b keeps decreasing by 2. if y is even, then
               b will decrease by 2 for y/2 times, each time inc a, then line
jnz 1 6        40 will go to 26, putting 2 in c but then going to right before
               42. Otherwise, y is odd, and it will decrease by 2 and inc a
dec b          exactly (y-1)/2 times, but end up with c = 1 and then go to 42
                
dec c           
                
jnz c -4        
       
inc a             x has same parity as y, so 2 for even y and 1 for odd y
                  is just 2 - parity(x)
jnz 1 -7       

cpy 2 b
  floor[y/2]  2   2-parity(x)    y
jnz c 2

jnz 1 4

dec b       52 will run exactly 2-parity(x) times, so at 54 b will be 0 if
            x is even and 1 if x is odd, so b will be parity(x)
dec c

jnz 1 -4

jnz 0 0
  floor(y/2)  parity(x)  0   y
out b

jnz a -19        goes to 20

jnz 1 -21        goes to 16

================

so want signal 0 1 0 1, so x must be even because 56 prints parity(x) the first
time. 58 - 38 = 20

so we have
floor(y/2)    parity(x)  <-- leftmost bit of y
floor(^/2)    parity(^)  <-- next bit of y
floor(^/2)    parity(^)  ...

ah so those are just the bits of y

last bit is definitely 1 when a is 0

16 has an jn c -5, but c = 0 after 54 (see above) so that gets skipped. Note we
still have the same d = y, so this whole thing starts over!

So basically we must have  x + 4 * 633 have bit pattern
of the form 10101010.
-}
main = do
    putStr "part 1 = "
    print $ head [ x | x <- [1..], valid (x + 4 * 633)]

bits x = rec x []
  where rec 0 acc = acc
        rec x acc = rec (x `div` 2) (x `mod` 2 : acc)

validBits [] = True
validBits (1:0:xs) = validBits xs
validBits _ = False

valid = validBits . bits

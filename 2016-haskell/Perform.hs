module Perform
(
  perform,
  parse,
  value,
  Val(A,B,C,D)
) where

-- for day 12 and day 23

import Text.Read (readMaybe)
import Data.List (splitAt)

data Ins = Cpy Val Val | Inc Int Val | Jnz Val Val | Tgl Val
data Val = A | B | C | D | Val Int

parse :: String -> Ins
parse ln = case cmd of
  "cpy" -> Cpy val1 val2
  "inc" -> Inc 1 val1
  "dec" -> Inc (-1) val1
  "jnz" -> Jnz val1 val2
  "tgl" -> Tgl val1
  _ -> undefined
  where (cmd:args) = words ln
        val1 = parseVal $ head args
        val2 = parseVal $ last args

parseVal s = case readMaybe s of
  Just i -> Val i
  Nothing -> parseReg s

parseReg :: String -> Val
parseReg "a" = A
parseReg "b" = B
parseReg "c" = C
parseReg "d" = D
parseReg _ = undefined

type Regs = (Int, Int, Int, Int)

value :: Val -> Regs -> Int
value (Val x) _ = x
value A (v,_,_,_) = v
value B (_,v,_,_) = v
value C (_,_,v,_) = v
value D (_,_,_,v) = v

perform :: [Ins] -> [Ins] -> Regs -> Regs
perform _ [] regs = regs
perform before rest@(Jnz v k : _) regs =
  performJump (value v regs) (value k regs) before rest regs
perform before rest@(Tgl v : _) regs =
  performToggle (value v regs) before rest regs
perform before (ins : after) regs =
  perform (ins:before) after (performOne ins regs)

performToggle k before rest regs
  | k >= 0 = perform (head tRest : before) (tail tRest) regs
  | otherwise = perform (head rest : tBefore) (tail rest) regs
  where tRest = toggleK rest k
        tBefore = toggleK before (abs k - 1)

toggleK xs k
  | k >= length xs = xs
  | otherwise = bef ++ [toggle x] ++ aft
  where (bef, x : aft) = splitAt k xs

performJump i k before rest regs
  | i == 0 = noop
  | k > length rest = noop
  | k > 0 = perform (reverse restK ++ before) restButK regs
  | k < negate (length before) = noop
  | k < 0 = perform beforeButK (reverse beforeK ++ rest) regs
  | otherwise = undefined
  where (restK, restButK) = splitAt k rest
        (beforeK, beforeButK) = splitAt (abs k) before
        noop = perform (head rest : before) (tail rest) regs

performOne (Cpy p q) regs = storeVal (value p regs) q regs
performOne (Inc i q) regs = addVal i q regs
performOne _ _ = undefined

storeVal i A (a,b,c,d) = (i,b,c,d)
storeVal i B (a,b,c,d) = (a,i,c,d)
storeVal i C (a,b,c,d) = (a,b,i,d)
storeVal i D (a,b,c,d) = (a,b,c,i)
storeVal _ (Val _) rs = rs

addVal i A (a,b,c,d) = (a+i,b,c,d)
addVal i B (a,b,c,d) = (a,b+i,c,d)
addVal i C (a,b,c,d) = (a,b,c+i,d)
addVal i D (a,b,c,d) = (a,b,c,d+i)
addVal _ (Val _) rs = rs

toggle :: Ins -> Ins
toggle (Inc i x) = Inc (negate i) x
toggle (Tgl x) = Inc 1 x
toggle (Jnz x y) = Cpy x y
toggle (Cpy x y) = Jnz x y

import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Text.Read (readMaybe)
import Data.List (splitAt)

main = do
    han <- openFile "input.txt" ReadMode
    conts <- hGetContents han
    let inss = map parse $ lines conts
        ans1 = perform [] inss (0,0,0,0)
        ans2 = perform [] inss (0,0,1,0)
    putStr "part 1 = "
    print $ value A ans1
    putStr "part 2 = "
    print $ value A ans2
    hClose han

data Reg = A | B | C | D deriving (Show)
data Ins = CpI Int Reg | CpR Reg Reg | Inc Reg | Dec Reg |
           JnzI Int Int | JnzR Reg Int deriving (Show)

parse :: String -> Ins
parse ln = case head ws of
  "cpy" -> parseCpy $ tail ws
  "inc" -> Inc reg
  "dec" -> Dec reg
  "jnz" -> parseJnz $ tail ws
  _ -> error "parse"
  where ws = words ln
        reg = parseReg (ws !! 1)

parseCpy :: [String] -> Ins
parseCpy [x, y] = case readMaybe x of
  Just i -> CpI i reg2
  Nothing -> CpR reg1 reg2
  where reg1 = parseReg x
        reg2 = parseReg y
parseCpy _ = error "parseCpy"

parseJnz :: [String] -> Ins
parseJnz [x, y] = case readMaybe x of
  Just i -> JnzI i k
  Nothing -> JnzR (parseReg x) k
  where k = read y
parseJnz _ = error "parseJnz"

parseReg :: String -> Reg
parseReg "a" = A
parseReg "b" = B
parseReg "c" = C
parseReg "d" = D
parseReg _ = error "parseReg"

type Regs = (Int, Int, Int, Int)

value :: Reg -> Regs -> Int
value A (v,_,_,_) = v
value B (_,v,_,_) = v
value C (_,_,v,_) = v
value D (_,_,_,v) = v

perform :: [Ins] -> [Ins] -> Regs -> Regs
perform _ [] regs = regs
perform before rest@(JnzI i k : _) regs =
  performJump i k before rest regs
perform before rest@(JnzR r k : _) regs =
  performJump (value r regs) k before rest regs
perform before (ins : after) regs =
  perform (ins:before) after (performOne ins regs)

performJump i k before rest regs
  | i == 0 = perform (ins:before) after regs
  | k > 0 = perform (reverse restK ++ before) restButK regs
  | k < 0 = perform beforeButK (reverse beforeK ++ rest) regs
  | otherwise = error "performJump"
  where (restK, restButK) = splitAt (abs k) rest
        (beforeK, beforeButK) = splitAt (abs k) before
        ins = head rest
        after = tail rest

performOne (CpI i r) regs = storeVal i r regs
performOne (CpR t r) regs = storeVal (value t regs) r regs
performOne (Inc r) regs = addVal 1 r regs
performOne (Dec r) regs = addVal (-1) r regs
performOne _ _ = error "performOne"

storeVal i A (a,b,c,d) = (i,b,c,d)
storeVal i B (a,b,c,d) = (a,i,c,d)
storeVal i C (a,b,c,d) = (a,b,i,d)
storeVal i D (a,b,c,d) = (a,b,c,i)

addVal i A (a,b,c,d) = (a+i,b,c,d)
addVal i B (a,b,c,d) = (a,b+i,c,d)
addVal i C (a,b,c,d) = (a,b,c+i,d)
addVal i D (a,b,c,d) = (a,b,c,d+i)

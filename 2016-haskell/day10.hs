import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (find)

data Step = Value Int Dest | Give Int Dest Dest deriving (Show)
data Dest = Bot Int | Output Int deriving (Show)

main = do
    han <- openFile "input.txt" ReadMode
    conts <- hGetContents han
    let lns = lines conts
        steps = map fromStr lns
        (doneDouble, output) = solve steps
        ans1 = case find wanted doneDouble of
            Just (a, b, c) -> a
            Nothing -> -1
        ans2 = product [i | (o,i) <- output, o `elem` [0,1,2]]
    putStr "part 1 = "
    print ans1
    putStr "part 2 = "
    print ans2
    hClose han

wanted (_, a, b) = (a, b) == (61, 17) || (a, b) == (17, 61)

fromStr :: String -> Step
fromStr xs = case head ws of
    "value" -> valueFromStr ws
    "bot" -> giveFromStr ws
    _ -> error "fromStr"
    where ws = words xs

valueFromStr :: [String] -> Step
valueFromStr ws = Value (read $ ws !! 1) (Bot $ read $ last ws)

destFromWs :: [String] -> Dest
destFromWs ["bot", n] = Bot (read n)
destFromWs ["output", n] = Output (read n)
destFromWs _ = error "destFromWs"

giveFromStr :: [String] -> Step
giveFromStr ws = Give (read $ ws !! 1) low high where
    low = destFromWs [ws !! 5, ws !! 6]
    high = destFromWs $ drop 10 ws

solve steps = rec steps [] IntMap.empty IntMap.empty [] []

type Pairs = [(Int, Int)]
type Triples = [(Int, Int, Int)]

rec :: [Step] -> [Step] -> IntMap Int -> IntMap (Int, Int) -> Triples -> Pairs -> (Triples, Pairs)
rec [] [] _ _ doneDouble output = (doneDouble, output)
rec [] later single double doneDouble output =
    rec later [] single double doneDouble output
rec ((Value i (Output o)):steps) later single double doneDouble output =
    rec steps later single double doneDouble ((o,i):output)
rec ((Value i (Bot b)):steps) later single double doneDouble output =
    case IntMap.lookup b single of
        Just v -> rec steps later (IntMap.delete b single)
                      (IntMap.insert b (v, i) double) doneDouble output
        Nothing -> rec steps later (IntMap.insert b i single) double doneDouble
                       output
rec (step@(Give b low high):steps) later single double doneDouble output =
    case IntMap.lookup b double of
        Just (x, y) -> rec (Value (min x y) low : Value (max x y) high : steps)
                           later single double ((b,x,y) : doneDouble) output
        Nothing -> rec steps (step:later) single double doneDouble output

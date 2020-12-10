-- | 

module Day08 where

import qualified Data.Map.Strict as Map
import Data.List.Split
import qualified Data.Vector as V
import Debug.Trace as D

exampleInput :: String
exampleInput = "nop +0\n\
\acc +1\n\
\jmp +4\n\
\acc +3\n\
\jmp -3\n\
\acc -99\n\
\acc +1\n\
\jmp -4\n\
\acc +6"

data State = State {accumulator :: Int, programCounter :: Int} deriving Show
data Op = Op {opCode :: String, opFunc :: Int -> State -> State}

instance Show Op where
  show (Op code _) = show code

incPC :: State -> State
incPC (State acc pc) = (State acc (succ pc))

nopFunc :: Int -> State -> State
nopFunc _ (State acc pc) = (State acc (succ pc))

accFunc :: Int -> State -> State
accFunc n (State acc pc) = (State (acc + n) (succ pc))

jmpFunc :: Int -> State -> State
jmpFunc n (State acc pc) = (State acc (pc + n))

nopOp :: Op
nopOp = (Op "nop" nopFunc)

accOp :: Op
accOp = (Op "acc" accFunc)

jmpOp :: Op
jmpOp = (Op "jmp" jmpFunc)

opCodes :: Map.Map String Op
opCodes = Map.fromList $ map (\op@(Op name _) -> (name, op)) [nopOp, accOp, jmpOp]

initialState :: State
initialState = (State 0 0)

parseLine :: String -> (Op, Int)
parseLine line =
  let (rawOp:(sign:rawVal):_) = splitOn " " line
      v = read rawVal :: Int
      val = if sign == '-' then v * (-1) else v
      Just op = Map.lookup rawOp opCodes
  in (op, val)

parse :: String -> [(Op, Int)]
parse str = map parseLine $ lines str

exampleInp :: [(Op, Int)]
exampleInp = parse exampleInput

-- using curried opFuncs instead of tuples would work and look fancier, but the
-- ability to print them out makes it easier to handle.
exec :: [(Op, Int)] -> State -> State
exec ops s@(State _ pc) =
  let ((Op _ f), v) = ops !! pc in f v s

incAt :: Int -> V.Vector Int -> V.Vector Int
incAt pos vec =
  let oldV = vec V.! pos
  in vec V.// [(pos, (succ oldV))]

-- -- the part1 func basically
runOnceAndAcc :: [(Op, Int)] -> State -> V.Vector Int -> Int
runOnceAndAcc ops s@(State acc pc) callCounts
  | pc >= (length ops) = error $ "terminated (pc:" ++ show pc ++ ", acc:" ++ show acc ++ ") " ++ (show callCounts) 
  | callCounts V.! pc > 0 = acc 
  | otherwise = runOnceAndAcc ops (exec ops s) (incAt pc callCounts)
  
runOnceAndAcc' :: [(Op, Int)] -> Int
runOnceAndAcc' ops =
  let callCounts = V.replicate (length ops) 0
  in runOnceAndAcc ops initialState callCounts

part1 :: IO ()
part1 = do
  raw <- readFile "data/day08.txt"
  let inp = parse raw
  print $ runOnceAndAcc' inp

--- part2
-- just going for naive impl for now. Just try to replace all jmps.

jmpToNop :: [(Op, Int)] -> Int -> [(Op,Int)]
jmpToNop (x@((Op "jmp" _), v):xs) n
  | n == 0 = ((nopOp, v):xs)
  | n < 0 = (x : jmpToNop xs n)
  | n > 0 = (x : jmpToNop xs (pred n)) -- let's skip this nop
jmpToNop (x:xs) n = (x : jmpToNop xs n)

part2 :: IO ()
part2 = do
  raw <- readFile "data/day08.txt"
  let inp = parse raw
  let countNops = length $ filter (\ ((Op code _), _) -> code == "jmp") inp
  -- ok this is ugly, but it will print the error with the right acc
  print $ map (\ c -> runOnceAndAcc' (jmpToNop inp c)) [0..(pred countNops)]
  
-- There are *just static jmps*, so we can turn the program into many directed graph where nodes
-- are segments of code and each nodes points toward another node.
-- I was so focused on Haskell that I missed a really easy way to solve this.
-- See :152 if you're not interested in my late night ramblings. 
--
--
-- 0: nop    node A (0 to 1)
-- 1: jmp 4  node A (0 to 1)
-- 2: nop
-- 3: nop
-- 4: nop    node B (4 to 5) 
-- 5: jmp 99 node B (4 to 5)
-- 6: nop
--
-- can be viewed as A ----> B
-- 
-- We *know* from the instructions that the one we're starting on is cyclic, since we're looking to
-- fix the infinite loop.
--
-- It's possible that some other graphs are pointing toward the starting graph (the one on which PC=0)
-- but we don't care about them because we can't reach them. Similarly, we can have graphs are totally
-- unconnected and we care even less about them.
--
-- So because we're looking to fix it by changing a single jump into a nop to terminate, we need to
-- look for a node that points to one that terminates. 
--
-- We have one more piece of info here, the nodes can be (partially? it's late, I guess yes) ordered
-- by their PC. In a given graph the lower the PC is, the earlier we are in the graph and thus the bigger the
-- amount of jmps there will be after. 
--
-- With that in mind, it means that we could:
--   - ignore all jumps which are not going to be evaluated
--   - take notes of all jumps we take during the evaluation.
--   - take remove the last one we have noted
--   - and our program would terminate.
--
-- Conveniently, we have this list after a first run, it's all the PCs we have visited at least once,
-- see `callCounts` in the code above. So it's the last jmp instruction where callCounts[pc] == 1 that
-- needs to be dropped. To reuse the instructions terminology, it's the "second-to-last" executed,
-- executed being the trick here, jmp that we need to drop.
--
-- But I'm focused on having fun with Haskell for now, so I'll just move to day 9 haha, I'm one day late already xD

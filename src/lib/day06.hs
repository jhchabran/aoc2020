-- | 

module Day06 where

import Data.List.Split
import qualified Data.Set as Set

exampleInput :: String
exampleInput = "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"

splitGroups :: String -> [String]
splitGroups = splitOn "\n\n"

answers :: String -> Set.Set Char
answers str =
  let f [] set = set
      f ('\n':xs) set = f xs set
      f (x:xs) set = f xs $ Set.insert x set
  in f str Set.empty

sumCounts :: String -> Int
sumCounts inp =
  let groups = splitGroups inp
  in sum $ map (Set.size . answers) groups

part1 :: IO ()
part1 = do
  raw <- readFile "data/day06.txt"
  print $ sumCounts raw
  
-- part2

answersInCommon :: String -> Int
answersInCommon str =
  let f [] set = set
      f (x:xs) set = f xs (Set.intersection (answers x) set)
      ans = filter (\s -> s /= "") (splitOn "\n" str) -- there is a final \n at the end of the file 
  in Set.size (f (tail ans) (answers $ head ans))

sumAnswersInCommon :: String -> Int
sumAnswersInCommon inp =
  let groups = splitGroups inp
  in sum $ map answersInCommon groups

part2 :: IO ()
part2 = do
  raw <- readFile "data/day06.txt"
  print $ sumAnswersInCommon raw


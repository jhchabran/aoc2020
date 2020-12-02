-- | 

module Day02 where

import Data.List.Split

-- 1-3 a: abcde
-- 1-3 b: cdefg
-- 2-9 c: ccccccccc

-- the provided example from the instructions
exampleInput :: [([Integer], Char, String)]
exampleInput = [([1..3], 'a', "abcde"),
                ([1..3], 'b', "cdefg"),
                ([2..9], 'c', "ccccccccc")]

countChar :: String -> Char -> Integer
countChar str c = foldl (\count char -> if char == c then (count + 1) else count) 0 str

isPasswordValid :: ([Integer], Char, String) -> Bool
isPasswordValid (boundaries, char, str) =
  let count = countChar str char
      inBoundaries = length (filter (== count) boundaries)
  in inBoundaries > 0

-- "2-5" -> [2,3,4,5]
parseInterval :: String -> [Integer]
parseInterval rawInterval =
  let rawBounds = splitOn "-" rawInterval
      bounds = map (\bound -> read bound :: Integer) rawBounds
  in [(head bounds)..(last bounds)] 
    
parseInput :: String -> ([Integer], Char, String)
parseInput rawInput =
  let segments = splitOn " " rawInput
      interval = parseInterval (head segments)
      char = head $ segments!!1
      password = segments!!2
  in (interval, char, password)

input :: IO [([Integer], Char, String)]
input = do
  raw <- readFile "data/Day02.txt"
  let somePasswords = map parseInput . lines $ raw
  return somePasswords
  
countValidPasswords :: [([Integer], Char, String)] -> Integer
countValidPasswords entries =
  foldl (\count entry -> if isPasswordValid entry then count + 1 else count) 0 entries


part1 :: IO ()
part1 = do
  inp <- input
  print $ countValidPasswords inp

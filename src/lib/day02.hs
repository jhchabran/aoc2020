-- |

-- Maybe learning about records would have helped here xD
-- Still, it's maybe a bit incovenient but nowhere incorrect.

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

-- Each policy actually describes two positions in the password, where 1 means the first character,
-- 2 means the second character, and so on. (Be careful; Toboggan Corporate Policies have no concept
-- of "index zero"!) Exactly one of these positions must contain the given letter.
-- Other occurrences of the letter are irrelevant for the purposes of policy enforcement.
parsePositions :: String -> (Integer, Integer)
parsePositions rawPositions =
  let raws = splitOn "-" rawPositions
      res = map (\position -> read position :: Integer) raws
  in (res!!0, res!!1)
  
parseInput2 :: String -> ((Integer, Integer), Char, String)
parseInput2 rawInput =
  let segments = splitOn " " rawInput
      positions = parsePositions (head segments)
      char = head $ segments!!1
      password = segments!!2
  in (positions, char, password)

input2 :: IO [((Integer, Integer), Char, String)]
input2 = do
  raw <- readFile "data/Day02.txt"
  let somePasswords = map parseInput2 . lines $ raw
  return somePasswords
  
invalidPassword2 :: ((Integer, Integer), Char, String)
invalidPassword2 = ((1,3), 'a', "aba")

invalidPassword2' :: ((Integer, Integer), Char, String)
invalidPassword2' = ((1,3), 'a', "bbb")

validPassword2 :: ((Integer, Integer), Char, String)
validPassword2 = ((1,3), 'a', "abb")

validPassword2' :: ((Integer, Integer), Char, String)
validPassword2' = ((1,3), 'a', "bba")

isPasswordValid2 :: ((Integer, Integer), Char, String) -> Bool
isPasswordValid2 ((p1, p2), c, str) =
  let a = str !! (fromIntegral p1 - 1)
      b = str !! (fromIntegral p2 - 1)
  in (a == c && b /= c) || (a /= c && b == c)

countValidPasswords2 :: [((Integer, Integer), Char, String)] -> Integer
countValidPasswords2 entries =
  foldl (\count entry -> if isPasswordValid2 entry then count + 1 else count) 0 entries

part2 :: IO ()
part2 = do
  inp <- input2
  print $ countValidPasswords2 inp
